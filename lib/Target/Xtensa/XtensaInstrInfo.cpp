#include "XtensaInstrInfo.h"
#include "XtensaTargetMachine.h"
#include "llvm/CodeGen/MachineConstantPool.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"

#define GET_INSTRINFO_CTOR_DTOR
#define GET_INSTRMAP_INFO
#include "XtensaGenInstrInfo.inc"

using namespace llvm;

/// Add a BDX memory reference for frame object FI to MIB.
static inline const MachineInstrBuilder &
addFrameReference(const MachineInstrBuilder &MIB, int FI) {
  MachineInstr *MI = MIB;
  MachineFunction &MF = *MI->getParent()->getParent();
  MachineFrameInfo &MFFrame = MF.getFrameInfo();
  const MCInstrDesc &MCID = MI->getDesc();
  MachineMemOperand::Flags Flags = MachineMemOperand::MONone;
  if (MCID.mayLoad())
    Flags |= MachineMemOperand::MOLoad;
  if (MCID.mayStore())
    Flags |= MachineMemOperand::MOStore;
  int64_t Offset = 0;
  unsigned Align = MFFrame.getObjectAlignment(FI);

  MachineMemOperand *MMO =
      MF.getMachineMemOperand(MachinePointerInfo::getFixedStack(MF, FI, Offset),
                              Flags, MFFrame.getObjectSize(FI), Align);
  return MIB.addImm(Offset).addFrameIndex(FI).addMemOperand(MMO);
}

XtensaInstrInfo::XtensaInstrInfo(XtensaSubtarget &sti)
    : XtensaGenInstrInfo(Xtensa::ADJCALLSTACKDOWN, Xtensa::ADJCALLSTACKUP),
      RI(sti), STI(sti) {}

unsigned XtensaInstrInfo::isLoadFromStackSlot(const MachineInstr &MI,
                                              int &FrameIndex) const {
  int OpCode = MI.getOpcode();
  if ((OpCode == Xtensa::L32I || OpCode == Xtensa::L32I_N) &&
      MI.getOperand(1).isFI() && MI.getOperand(2).getImm() == 0 &&
      MI.getOperand(3).getReg() == 0) {
    FrameIndex = MI.getOperand(1).getIndex();
    return MI.getOperand(0).getReg();
  }
  return 0;
}

unsigned XtensaInstrInfo::isStoreToStackSlot(const MachineInstr &MI,
                                             int &FrameIndex) const {
  int OpCode = MI.getOpcode();
  if ((OpCode == Xtensa::S32I || OpCode == Xtensa::S32I_N) &&
      MI.getOperand(1).isFI() && MI.getOperand(2).getImm() == 0 &&
      MI.getOperand(3).getReg() == 0) {
    FrameIndex = MI.getOperand(1).getIndex();
    return MI.getOperand(0).getReg();
  }
  return 0;
}

/// Adjust SP by Amount bytes.
void XtensaInstrInfo::adjustStackPtr(unsigned SP, int64_t Amount,
                                     MachineBasicBlock &MBB,
                                     MachineBasicBlock::iterator I) const {
  DebugLoc DL = I != MBB.end() ? I->getDebugLoc() : DebugLoc();

  if (Amount == 0)
    return;

  MachineRegisterInfo &RegInfo = MBB.getParent()->getRegInfo();
  const TargetRegisterClass *RC = &Xtensa::ARRegClass;

  // create virtual reg to store immediate
  unsigned Reg = RegInfo.createVirtualRegister(RC);

  if (Amount > 1 && Amount <= 15)
    BuildMI(MBB, I, DL, get(Xtensa::ADDI_N), Reg).addReg(SP).addImm(Amount);
  else if (isInt<8>(Amount)) // addi sp, sp, amount
    BuildMI(MBB, I, DL, get(Xtensa::ADDI), Reg).addReg(SP).addImm(Amount);
  else { // Expand immediate that doesn't fit in 12-bit.
    unsigned Reg1;
    loadImmediate(MBB, I, &Reg1, Amount);
    BuildMI(MBB, I, DL, get(Xtensa::ADD), Reg)
        .addReg(SP)
        .addReg(Reg1, RegState::Kill);
  }
  BuildMI(MBB, I, DL, get(Xtensa::MOVSP), SP).addReg(Reg, RegState::Kill);
}

unsigned XtensaInstrInfo::GetInstSizeInBytes(MachineInstr *MI) const {
  switch (MI->getOpcode()) {
  case TargetOpcode::INLINEASM: { // Inline Asm: Variable size.
    const MachineFunction *MF = MI->getParent()->getParent();
    const char *AsmStr = MI->getOperand(0).getSymbolName();
    return getInlineAsmLength(AsmStr, *MF->getTarget().getMCAsmInfo());
  }
    // TODO
    //  case Xtensa::CONSTPOOL_ENTRY:
    // If this machine instr is a constant pool entry, its size is recorded as
    // operand #2.
    //    return MI->getOperand(2).getImm();
  default:
    return MI->getDesc().getSize();
  }
}

bool XtensaInstrInfo::analyzeBranch(MachineBasicBlock &MBB,
                                    MachineBasicBlock *&TBB,
                                    MachineBasicBlock *&FBB,
                                    SmallVectorImpl<MachineOperand> &Cond,
                                    bool AllowModify = false) const {
  // Most of the code and comments here are boilerplate.

  // Start from the bottom of the block and work up, examining the
  // terminator instructions.
  MachineBasicBlock::iterator I = MBB.end();
  while (I != MBB.begin()) {
    --I;
    if (I->isDebugValue())
      continue;

    // Working from the bottom, when we see a non-terminator instruction, we're
    // done.
    if (!isUnpredicatedTerminator(*I))
      break;

    // A terminator that isn't a branch can't easily be handled by this
    // analysis.
    SmallVector<MachineOperand, 4> ThisCond;
    ThisCond.push_back(MachineOperand::CreateImm(0));
    const MachineOperand *ThisTarget;
    if (!isBranch(I, ThisCond, ThisTarget))
      return true;

    // Can't handle indirect branches.
    if (!ThisTarget->isMBB())
      return true;

    //if (ThisCond[0].getImm() == Xtensa::JX)
    //  return true;

    if (ThisCond[0].getImm() == Xtensa::J) {
      // Handle unconditional branches.
      if (!AllowModify) {
        TBB = ThisTarget->getMBB();
        continue;
      }

      // If the block has any instructions after a JMP, delete them.
      while (std::next(I) != MBB.end())
        std::next(I)->eraseFromParent();

      Cond.clear();
      FBB = 0;

      // Delete the JMP if it's equivalent to a fall-through.
      /*We can't do this now because the BBs can still be rearranged
            if (MBB.isLayoutSuccessor(ThisTarget->getMBB())) {
              TBB = 0;
              I->eraseFromParent();
              I = MBB.end();
              continue;
            }
      */

      // TBB is used to indicate the unconditinal destination.
      TBB = ThisTarget->getMBB();
      continue;
    }

    // Working from the bottom, handle the first conditional branch.
    if (Cond.empty()) {
      // FIXME: add X86-style branch swap
      FBB = TBB;
      TBB = ThisTarget->getMBB();
      Cond.push_back(MachineOperand::CreateImm(ThisCond[0].getImm()));

      // push remaining operands
      for (unsigned int i = 0; i < (I->getNumExplicitOperands()-1); i++)
        Cond.push_back(I->getOperand(i));

      continue;
    }

    // Handle subsequent conditional branches.
    assert(Cond.size() <= 4);
    assert(TBB);

    // Only handle the case where all conditional branches branch to the same
    // destination.
    if (TBB != ThisTarget->getMBB())
      return true;

    // If the conditions are the same, we can leave them alone.
    unsigned OldCond = Cond[0].getImm();
    if (OldCond == ThisCond[0].getImm())
      continue;

    // FIXME: Try combining conditions like X86 does.
  }

  return false;
}

unsigned XtensaInstrInfo::removeBranch(MachineBasicBlock &MBB,
                                       int *BytesRemoved) const {
  assert(!BytesRemoved && "code size not handled");

  // Most of the code and comments here are boilerplate.
  MachineBasicBlock::iterator I = MBB.end();
  unsigned Count = 0;

  while (I != MBB.begin()) {
    --I;
    if (I->isDebugValue())
      continue;
    SmallVector<MachineOperand, 4> Cond;
    Cond.push_back(MachineOperand::CreateImm(0));
    const MachineOperand *Target;
    if (!isBranch(I, Cond, Target))
      break;
    if (!Target->isMBB())
      break;
    // Remove the branch.
    I->eraseFromParent();
    I = MBB.end();
    ++Count;
  }

  return Count;
}

unsigned XtensaInstrInfo::insertBranch(
    MachineBasicBlock &MBB, MachineBasicBlock *TBB, MachineBasicBlock *FBB,
    ArrayRef<MachineOperand> Cond, const DebugLoc &DL, int *BytesAdded) const {
  assert(!BytesAdded && "code size not handled");
  if (FBB) {
    // Need to build two branches then
    // one to branch to TBB on Cond
    // and a second one immediately after to unconditionally jump to FBB
    unsigned count = InsertBranchAtInst(MBB, MBB.end(), TBB, Cond, DL);
    BuildMI(&MBB, DL, get(Xtensa::J)).addMBB(FBB);
    count++;
    return count;
  }
  // This function inserts the branch at the end of the MBB
  return InsertBranchAtInst(MBB, MBB.end(), TBB, Cond, DL);
}

unsigned XtensaInstrInfo::InsertConstBranchAtInst(MachineBasicBlock &MBB,
                                                  MachineInstr *I,
                                                  int64_t offset,
                                                  ArrayRef<MachineOperand> Cond,
                                                  DebugLoc DL) const {
  // Shouldn't be a fall through.
  assert(&MBB && "InsertBranch must not be told to insert a fallthrough");
  assert(Cond.size() <= 4 &&
         "Xtensa branch conditions have less than four components!");

  if (Cond.empty() || (Cond[0].getImm() == Xtensa::J)) {
    // Unconditional branch
    BuildMI(MBB, I, DL, get(Xtensa::J)).addImm(offset);
    return 1;
  }

  unsigned Count = 0;
  unsigned BR_C = Cond[0].getImm();
  unsigned BRANCH_TYPE = BranchType(BR_C);
  switch (BRANCH_TYPE) {
  case Xtensa::CBRANCH_RR:
    BuildMI(MBB, I, DL, get(BR_C))
        .addImm(offset)
        .addReg(Cond[1].getReg())
        .addReg(Cond[2].getReg());
    break;
  case Xtensa::CBRANCH_RI:
    BuildMI(MBB, I, DL, get(BR_C))
        .addImm(offset)
        .addReg(Cond[1].getReg())
        .addImm(Cond[2].getImm());
    break;
  case Xtensa::CBRANCH_RZ:
    BuildMI(MBB, I, DL, get(BR_C))
        .addImm(offset)
        .addReg(Cond[1].getReg());
    break;
  case Xtensa::CBRANCH_B:
    BuildMI(MBB, I, DL, get(BR_C)).addImm(offset).addReg(Cond[1].getReg());
    break;
  default:
    llvm_unreachable("Invalid branch type!");
  }
  ++Count;
  return Count;
}

unsigned XtensaInstrInfo::InsertBranchAtInst(MachineBasicBlock &MBB,
                                             MachineBasicBlock::iterator I,
                                             MachineBasicBlock *TBB,
                                             ArrayRef<MachineOperand> Cond,
                                             const DebugLoc &DL) const {
  // Shouldn't be a fall through.
  assert(TBB && "InsertBranch must not be told to insert a fallthrough");
  assert(Cond.size() <= 4 &&
         "Xtensa branch conditions have less than four components!");

  if (Cond.empty() || (Cond[0].getImm() == Xtensa::J)) {
    // Unconditional branch
    BuildMI(MBB, I, DL, get(Xtensa::J)).addMBB(TBB);
    return 1;
  }

  unsigned Count = 0;
  unsigned BR_C = Cond[0].getImm();
  unsigned BRANCH_TYPE = BranchType(BR_C);
  switch (BRANCH_TYPE) {
  case Xtensa::CBRANCH_RR:
    BuildMI(MBB, I, DL, get(BR_C))
        .addReg(Cond[1].getReg())
        .addReg(Cond[2].getReg())
        .addMBB(TBB);
    break;
  case Xtensa::CBRANCH_RI:
    BuildMI(MBB, I, DL, get(BR_C))
        .addReg(Cond[1].getReg())
        .addImm(Cond[2].getImm())
        .addMBB(TBB);
    break;
  case Xtensa::CBRANCH_RZ:
    BuildMI(MBB, I, DL, get(BR_C))
        .addReg(Cond[1].getReg())
        .addMBB(TBB);
    break;
  case Xtensa::CBRANCH_B:
    BuildMI(MBB, I, DL, get(BR_C))
		.addReg(Cond[1].getReg())
        .addMBB(TBB);
    break;
  default:
    llvm_unreachable("Invalid branch type!");
  }
  ++Count;
  return Count;
}

void XtensaInstrInfo::copyPhysReg(MachineBasicBlock &MBB,
                                  MachineBasicBlock::iterator MBBI,
                                  const DebugLoc &DL, unsigned DestReg,
                                  unsigned SrcReg, bool KillSrc) const {

  unsigned Opcode;

  // when we are copying a phys reg we want the bits for fp
  if (Xtensa::ARRegClass.contains(DestReg, SrcReg))
    Opcode = Xtensa::MOV_N;
  else if (STI.hasF() && Xtensa::FPRRegClass.contains(SrcReg) &&
           Xtensa::FPRRegClass.contains(DestReg))
    Opcode = Xtensa::MOV_S;
  else if (STI.hasF() && Xtensa::FPRRegClass.contains(SrcReg) &&
           Xtensa::ARRegClass.contains(DestReg))
    Opcode = Xtensa::RFR;
  else if (STI.hasF() && Xtensa::ARRegClass.contains(SrcReg) &&
           Xtensa::FPRRegClass.contains(DestReg))
    Opcode = Xtensa::WFR;
  /*
  else if (Xtensa::ARRegClass.contains(SrcReg) &&
           Xtensa::SARLRegClass.contains(DestReg))
    Opcode = Xtensa::SSL;
  else if (Xtensa::ARRegClass.contains(SrcReg) &&
           Xtensa::SARRRegClass.contains(DestReg))
    Opcode = Xtensa::SSR;
  */

  /*
  else if (Xtensa::FP32BitRegClass.contains(DestReg, SrcReg))
  {
    Opcode = Xtensa::FSGNJ_S;
    BuildMI(MBB, MBBI, DL, get(Opcode), DestReg)
      .addReg(SrcReg, getKillRegState(KillSrc))
      .addReg(SrcReg, getKillRegState(KillSrc));
    return;
  }
  else if (Xtensa::FP64BitRegClass.contains(DestReg, SrcReg))
  {
    Opcode = Xtensa::FSGNJ_D;
    BuildMI(MBB, MBBI, DL, get(Opcode), DestReg)
      .addReg(SrcReg, getKillRegState(KillSrc))
      .addReg(SrcReg, getKillRegState(KillSrc));
    return;
  }
  else if(Xtensa::FP32BitRegClass.contains(SrcReg) &&
           Xtensa::GR32BitRegClass.contains(DestReg)){
    Opcode = STI.isRV64() ? Xtensa::FMV_X_S64 : Xtensa::FMV_X_S;
    BuildMI(MBB, MBBI, DL, get(Opcode), DestReg)
      .addReg(SrcReg, getKillRegState(KillSrc));
    return;
  }
  else if(Xtensa::FP32BitRegClass.contains(SrcReg) &&
           Xtensa::GR64BitRegClass.contains(DestReg)){
    Opcode = Xtensa::FMV_X_S64;
    BuildMI(MBB, MBBI, DL, get(Opcode), DestReg)
      .addReg(SrcReg, getKillRegState(KillSrc));
    return;
  }
  else if(Xtensa::FP64BitRegClass.contains(SrcReg) &&
           Xtensa::GR64BitRegClass.contains(DestReg)){
    Opcode = Xtensa::FMV_X_D;
    BuildMI(MBB, MBBI, DL, get(Opcode), DestReg)
      .addReg(SrcReg, getKillRegState(KillSrc));
    return;
  }
  else if(Xtensa::FP32BitRegClass.contains(DestReg) &&
           Xtensa::GR32BitRegClass.contains(SrcReg)){
    Opcode = STI.isRV64() ? Xtensa::FMV_S_X64 : Xtensa::FMV_S_X;
    BuildMI(MBB, MBBI, DL, get(Opcode), DestReg)
      .addReg(SrcReg, getKillRegState(KillSrc));
    return;
  }
  else if(Xtensa::FP64BitRegClass.contains(DestReg) &&
           Xtensa::GR64BitRegClass.contains(SrcReg))
  {
    Opcode = Xtensa::FMV_D_X;
    BuildMI(MBB, MBBI, DL, get(Opcode), DestReg)
      .addReg(SrcReg, getKillRegState(KillSrc));
    return;
  }
  else if(Xtensa::FP64BitRegClass.contains(DestReg) &&
           Xtensa::FP32BitRegClass.contains(SrcReg))
  {
    Opcode = Xtensa::FCVT_D_S_RDY;
    BuildMI(MBB, MBBI, DL, get(Opcode), DestReg)
      .addReg(SrcReg, getKillRegState(KillSrc));
    return;
  }
  else if(Xtensa::FP32BitRegClass.contains(DestReg) &&
           Xtensa::FP64BitRegClass.contains(SrcReg)){
    Opcode = Xtensa::FCVT_S_D_RDY;
    BuildMI(MBB, MBBI, DL, get(Opcode), DestReg)
      .addReg(SrcReg, getKillRegState(KillSrc));
    return;
  }
   */
  else
    llvm_unreachable("Impossible reg-to-reg copy");

  BuildMI(MBB, MBBI, DL, get(Opcode), DestReg)
      .addReg(SrcReg, getKillRegState(KillSrc));
}

void XtensaInstrInfo::storeRegToStackSlot(MachineBasicBlock &MBB,
                                          MachineBasicBlock::iterator MBBI,
                                          unsigned SrcReg, bool isKill,
                                          int FrameIdx,
                                          const TargetRegisterClass *RC,
                                          const TargetRegisterInfo *TRI) const {
  DebugLoc DL = MBBI != MBB.end() ? MBBI->getDebugLoc() : DebugLoc();

  // Callers may expect a single instruction, so keep 128-bit moves
  // together for now and lower them after register allocation.
  unsigned LoadOpcode, StoreOpcode;
  getLoadStoreOpcodes(RC, LoadOpcode, StoreOpcode, FrameIdx);
  addFrameReference(BuildMI(MBB, MBBI, DL, get(StoreOpcode))
                        .addReg(SrcReg, getKillRegState(isKill)),
                    FrameIdx);
}

void XtensaInstrInfo::loadRegFromStackSlot(
    MachineBasicBlock &MBB, MachineBasicBlock::iterator MBBI, unsigned DestReg,
    int FrameIdx, const TargetRegisterClass *RC,
    const TargetRegisterInfo *TRI) const {
  DebugLoc DL = MBBI != MBB.end() ? MBBI->getDebugLoc() : DebugLoc();

  // Callers may expect a single instruction, so keep 128-bit moves
  // together for now and lower them after register allocation.
  unsigned LoadOpcode, StoreOpcode;
  getLoadStoreOpcodes(RC, LoadOpcode, StoreOpcode, FrameIdx);
  addFrameReference(BuildMI(MBB, MBBI, DL, get(LoadOpcode), DestReg), FrameIdx);
}

bool XtensaInstrInfo::reverseBranchCondition(
    SmallVectorImpl<MachineOperand> &Cond) const {
  assert(Cond.size() <= 4 && "Invalid branch condition!");
  // Only need to switch the condition code, not the registers
  switch (Cond[0].getImm()) {
  case Xtensa::BEQ:
    Cond[0].setImm(Xtensa::BNE);
    return false;
  case Xtensa::BNE:
    Cond[0].setImm(Xtensa::BEQ);
    return false;
  case Xtensa::BLT:
    Cond[0].setImm(Xtensa::BGE);
    return false;
  case Xtensa::BGE:
    Cond[0].setImm(Xtensa::BLT);
    return false;
  case Xtensa::BLTU:
    Cond[0].setImm(Xtensa::BGEU);
    return false;
  case Xtensa::BGEU:
    Cond[0].setImm(Xtensa::BLTU);
    return false;

  case Xtensa::BGT:
    Cond[0].setImm(Xtensa::BLE);
    return false;
  case Xtensa::BGTU:
    Cond[0].setImm(Xtensa::BLEU);
  return false;
  case Xtensa::BLE:
    Cond[0].setImm(Xtensa::BGT);
  return false;
  case Xtensa::BLEU:
    Cond[0].setImm(Xtensa::BGTU);
  return false;

  case Xtensa::BEQI:
    Cond[0].setImm(Xtensa::BNEI);
    return false;
  case Xtensa::BNEI:
    Cond[0].setImm(Xtensa::BEQI);
    return false;
  case Xtensa::BGEI:
    Cond[0].setImm(Xtensa::BLTI);
    return false;
  case Xtensa::BLTI:
    Cond[0].setImm(Xtensa::BGEI);
    return false;
  case Xtensa::BGEUI:
    Cond[0].setImm(Xtensa::BLTUI);
    return false;
  case Xtensa::BLTUI:
    Cond[0].setImm(Xtensa::BGEUI);
    return false;

  case Xtensa::BEQZ:
    Cond[0].setImm(Xtensa::BNEZ);
    return false;
  case Xtensa::BNEZ:
    Cond[0].setImm(Xtensa::BEQZ);
    return false;
  case Xtensa::BLTZ:
    Cond[0].setImm(Xtensa::BGEZ);
    return false;
  case Xtensa::BGEZ:
    Cond[0].setImm(Xtensa::BLTZ);
    return false;


  case Xtensa::BFs:
    Cond[0].setImm(Xtensa::BTs);
    return false;
  case Xtensa::BTs:
    Cond[0].setImm(Xtensa::BFs);
    return false;
  default:
    llvm_unreachable("Invalid branch condition!");
  }
}

unsigned XtensaInstrInfo::BranchType(unsigned OpCode) const {
  switch (OpCode) {
  case Xtensa::J:
  case Xtensa::JX:
  case Xtensa::BR_JT:
    return Xtensa::UBRANCH;
  case Xtensa::BEQ:
  case Xtensa::BNE:
  case Xtensa::BLT:
  case Xtensa::BLTU:
  case Xtensa::BGE:
  case Xtensa::BGEU:
  case Xtensa::BGT:
  case Xtensa::BGTU:
  case Xtensa::BLE:
  case Xtensa::BLEU:
    return Xtensa::CBRANCH_RR;

  case Xtensa::BEQI:
  case Xtensa::BNEI:
  case Xtensa::BLTI:
  case Xtensa::BLTUI:
  case Xtensa::BGEI:
  case Xtensa::BGEUI:
    return Xtensa::CBRANCH_RI;

  case Xtensa::BEQZ:
  case Xtensa::BNEZ:
  case Xtensa::BLTZ:
  case Xtensa::BGEZ:
    return Xtensa::CBRANCH_RZ;

  case Xtensa::BTs:
  case Xtensa::BFs:
    return Xtensa::CBRANCH_B;

  default:
    llvm_unreachable("Unknown branch opcode!");
    return 0;
  }
}

bool XtensaInstrInfo::isBranch(const MachineBasicBlock::iterator &MI,
                               SmallVectorImpl<MachineOperand> &Cond,
                               const MachineOperand *&Target) const {
  unsigned OpCode = MI->getOpcode();
  switch (OpCode) {
  case Xtensa::J:
  case Xtensa::JX:
  case Xtensa::BR_JT:
    //    case Xtensa::CALL0:
    //    case Xtensa::CALLX0:
    //Cond[0].setImm(Xtensa::UBRANCH);
    Cond[0].setImm(OpCode);
    Target = &MI->getOperand(0);
    return true;
  case Xtensa::BEQ:
  case Xtensa::BNE:
  case Xtensa::BLT:
  case Xtensa::BLTU:
  case Xtensa::BGE:
  case Xtensa::BGEU:
  case Xtensa::BGT:
  case Xtensa::BGTU:
  case Xtensa::BLE:
  case Xtensa::BLEU:
    //Cond[0].setImm(Xtensa::CBRANCH_RR);
    Cond[0].setImm(OpCode);
    Target = &MI->getOperand(2);
    return true;

  case Xtensa::BEQI:
  case Xtensa::BNEI:
  case Xtensa::BLTI:
  case Xtensa::BLTUI:
  case Xtensa::BGEI:
  case Xtensa::BGEUI:
    //Cond[0].setImm(Xtensa::CBRANCH_RI);
    Cond[0].setImm(OpCode);
    Target = &MI->getOperand(2);
    return true;

  case Xtensa::BEQZ:
  case Xtensa::BNEZ:
  case Xtensa::BLTZ:
  case Xtensa::BGEZ:
    //Cond[0].setImm(Xtensa::CBRANCH_RZ);
    Cond[0].setImm(OpCode);
    Target = &MI->getOperand(1);
    return true;

  case Xtensa::BTs:
  case Xtensa::BFs:
    //Cond[0].setImm(Xtensa::CBRANCH_B);
    Cond[0].setImm(OpCode);
    Target = &MI->getOperand(1);
    return true;

  default:
    assert(!MI->getDesc().isBranch() && "Unknown branch opcode");
    return false;
  }
}

void XtensaInstrInfo::getLoadStoreOpcodes(const TargetRegisterClass *RC,
                                          unsigned &LoadOpcode,
                                          unsigned &StoreOpcode,
                                          int64_t offset) const {
  if (RC == &Xtensa::ARRegClass) {
    if (offset >= 0 && offset <= 60) {
      LoadOpcode = Xtensa::L32I_N;
      StoreOpcode = Xtensa::S32I_N;
    } else /* if (offset >= 0  &&  offset <= 1020) */
    {
      LoadOpcode = Xtensa::L32I;
      StoreOpcode = Xtensa::S32I;
    }
  } else if (RC == &Xtensa::FPRRegClass) {
    LoadOpcode = Xtensa::L32F;
    StoreOpcode = Xtensa::S32F;
  }
  /* TODO
  else if (RC == &Xtensa::FP32RegClass)
  {
    LoadOpcode = Xtensa::FLW;
    StoreOpcode = Xtensa::FSW;
  }
   */
  else
    llvm_unreachable("Unsupported regclass to load or store");
}

void XtensaInstrInfo::loadImmediate(MachineBasicBlock &MBB,
                                    MachineBasicBlock::iterator MBBI,
                                    unsigned *Reg, int64_t Value) const {
  DebugLoc DL = MBBI != MBB.end() ? MBBI->getDebugLoc() : DebugLoc();
  MachineRegisterInfo &RegInfo = MBB.getParent()->getRegInfo();
  const TargetRegisterClass *RC = &Xtensa::ARRegClass;

  // create virtual reg to store immediate
  *Reg = RegInfo.createVirtualRegister(RC);
  if (Value >= 32 && Value <= 95) {
    BuildMI(MBB, MBBI, DL, get(Xtensa::MOVI_N), *Reg).addImm(Value);
  } else if (Value >= -2048 && Value <= 2047) {
    BuildMI(MBB, MBBI, DL, get(Xtensa::MOVI), *Reg).addImm(Value);
  } else if (Value >= -32768 && Value <= 32767) {
    int Low = Value & 0xFF;
    int High = Value & ~0xFF;

    BuildMI(MBB, MBBI, DL, get(Xtensa::MOVI), *Reg).addImm(Low);
    BuildMI(MBB, MBBI, DL, get(Xtensa::ADDMI), *Reg).addReg(*Reg).addImm(High);
  } else if (Value >= -4294967296LL && Value <= 4294967295LL) {
    // 32 bit arbirary constant
    MachineConstantPool *MCP = MBB.getParent()->getConstantPool();
    uint64_t UVal = ((uint64_t) Value) & 0xFFFFFFFFLL;
    const Constant *CVal = ConstantInt::get(
        Type::getInt32Ty(MBB.getParent()->getFunction().getContext()), UVal,
        false);
	unsigned Idx = MCP->getConstantPoolIndex(CVal, 2U);
//	MCSymbol MSym
    BuildMI(MBB, MBBI, DL, get(Xtensa::L32R), *Reg).addConstantPoolIndex(Idx);
  } else {
      // use L32R to let assembler load immediate best
      // TODO replace to L32R
      llvm_unreachable("Unsupported load immediate value");
      //    BuildMI(MBB, MBBI, DL, get(Xtensa::LI), *Reg).addImm(Value);
    }
}

