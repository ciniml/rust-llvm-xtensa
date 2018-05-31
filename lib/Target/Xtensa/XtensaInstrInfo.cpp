#include "XtensaInstrInfo.h"
#include "XtensaTargetMachine.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"

#define GET_INSTRINFO_CTOR_DTOR
#define GET_INSTRMAP_INFO
#include "XtensaGenInstrInfo.inc"

using namespace llvm;

/// Add a BDX memory reference for frame object FI to MIB.
static inline const MachineInstrBuilder &
addFrameReference(const MachineInstrBuilder &MIB, int FI) 
{
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
    Flags, MFFrame.getObjectSize(FI),
    Align);
  return MIB.addImm(Offset).addFrameIndex(FI).addMemOperand(MMO);
}

XtensaInstrInfo::XtensaInstrInfo(XtensaSubtarget &sti)
  : XtensaGenInstrInfo(Xtensa::ADJCALLSTACKDOWN, Xtensa::ADJCALLSTACKUP),
    RI(sti), STI(sti) 
{
}

unsigned XtensaInstrInfo::isLoadFromStackSlot(const MachineInstr &MI,
                                               int &FrameIndex) const 
{
  int OpCode = MI.getOpcode();
  if ((OpCode == Xtensa::L32I  ||  OpCode == Xtensa::L32I_N) &&
      MI.getOperand(1).isFI() &&
      MI.getOperand(2).getImm() == 0 &&
      MI.getOperand(3).getReg() == 0)
  {
    FrameIndex = MI.getOperand(1).getIndex();
    return MI.getOperand(0).getReg();
  }
  return 0;  
}

unsigned XtensaInstrInfo::isStoreToStackSlot(const MachineInstr &MI,
                                              int &FrameIndex) const 
{
  int OpCode = MI.getOpcode();
  if ((OpCode == Xtensa::S32I  ||  OpCode == Xtensa::S32I_N) &&
      MI.getOperand(1).isFI() &&
      MI.getOperand(2).getImm() == 0 &&
      MI.getOperand(3).getReg() == 0)
  {
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

  if (Amount > 1  &&  Amount <= 15)
    BuildMI(MBB, I, DL, get(Xtensa::ADDI_N), SP).addReg(SP).addImm(Amount);
  else if (isInt<8>(Amount))// addi sp, sp, amount
    BuildMI(MBB, I, DL, get(Xtensa::ADDI), SP).addReg(SP).addImm(Amount);
  else 
  { // Expand immediate that doesn't fit in 12-bit.
    unsigned Reg;
    loadImmediate(MBB, I, &Reg, Amount);
    BuildMI(MBB, I, DL, get(Xtensa::ADD), SP).addReg(SP).addReg(Reg, RegState::Kill);
  }
}

unsigned XtensaInstrInfo::GetInstSizeInBytes(MachineInstr *MI) const 
{
  switch (MI->getOpcode()) 
  {
    /* TODO
    case  TargetOpcode::INLINEASM: 
    { // Inline Asm: Variable size.
      const MachineFunction *MF = MI->getParent()->getParent();
      const char *AsmStr = MI->getOperand(0).getSymbolName();
      return getInlineAsmLength(AsmStr, *MF->getTarget().getMCAsmInfo());
    }
  
     */ 
    default:  return MI->getDesc().getSize();
  }
}

bool XtensaInstrInfo::analyzeBranch(MachineBasicBlock &MBB,
                                     MachineBasicBlock *&TBB,
                                     MachineBasicBlock *&FBB,
                                     SmallVectorImpl<MachineOperand> &Cond,
                                     bool AllowModify = false) const 
{
  // Most of the code and comments here are boilerplate.

  // Start from the bottom of the block and work up, examining the
  // terminator instructions.
  MachineBasicBlock::iterator I = MBB.end();
  while (I != MBB.begin()) 
  {
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

    if (ThisCond[0].getImm() == Xtensa::CCMASK_ANY) 
    {
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
      //push remaining operands
      for (unsigned int i=0; i<(I->getNumExplicitOperands()); i++)
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

unsigned XtensaInstrInfo::removeBranch(MachineBasicBlock &MBB, int *BytesRemoved) const 
{
  assert(!BytesRemoved && "code size not handled");

  // Most of the code and comments here are boilerplate.
  MachineBasicBlock::iterator I = MBB.end();
  unsigned Count = 0;

  while (I != MBB.begin()) 
  {
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

unsigned
XtensaInstrInfo::insertBranch(MachineBasicBlock &MBB, MachineBasicBlock *TBB,
                               MachineBasicBlock *FBB,
                               ArrayRef<MachineOperand> Cond,
                               const DebugLoc &DL,
                               int *BytesAdded) const 
{
  assert(!BytesAdded && "code size not handled");
  if (FBB) 
  {
    //Need to build two branches then
    //one to branch to TBB on Cond
    //and a second one immediately after to unconditionally jump to FBB
    unsigned count = InsertBranchAtInst(MBB, MBB.end(), TBB, Cond, DL);
    BuildMI(&MBB, DL, get(Xtensa::J)).addMBB(FBB);
    count++;
    return count;
  }
  //This function inserts the branch at the end of the MBB
  return InsertBranchAtInst(MBB, MBB.end(), TBB, Cond, DL);
}
unsigned
XtensaInstrInfo::InsertConstBranchAtInst(MachineBasicBlock &MBB, 
                               MachineInstr *I, int64_t offset,
                               ArrayRef<MachineOperand> Cond,
                               DebugLoc DL) const 
{
  // Shouldn't be a fall through.
  assert(&MBB && "InsertBranch must not be told to insert a fallthrough");
  assert(Cond.size() <= 4 &&
         "Xtensa branch conditions have less than four components!");

  if (Cond.empty() || Cond[0].getImm() == Xtensa::CCMASK_ANY) 
  {
    // Unconditional branch
    BuildMI(MBB, I, DL, get(Xtensa::J)).addImm(offset);
    return 1;
  }

  // Conditional branch.
  unsigned Count = 0;
  unsigned CC = Cond[0].getImm();
  switch(CC) 
  {
    case Xtensa::CCMASK_CMP_EQ:
      BuildMI(MBB, I, DL, get(Xtensa::BEQ)).addImm(offset).addReg(Cond[2].getReg())
          .addReg(Cond[3].getReg());
      break;
    case Xtensa::CCMASK_CMP_NE:
      BuildMI(MBB, I, DL, get(Xtensa::BNE)).addImm(offset).addReg(Cond[2].getReg())
          .addReg(Cond[3].getReg());
      break;
    case Xtensa::CCMASK_CMP_LT:
      BuildMI(MBB, I, DL, get(Xtensa::BLT)).addImm(offset).addReg(Cond[2].getReg())
          .addReg(Cond[3].getReg());
      break;
    case (Xtensa::CCMASK_CMP_LT | Xtensa::CCMASK_CMP_UO):
      BuildMI(MBB, I, DL, get(Xtensa::BLTU)).addImm(offset).addReg(Cond[2].getReg())
          .addReg(Cond[3].getReg());
      break;
    case Xtensa::CCMASK_CMP_GE:
      BuildMI(MBB, I, DL, get(Xtensa::BGE)).addImm(offset).addReg(Cond[2].getReg())
          .addReg(Cond[3].getReg());
      break;
    case (Xtensa::CCMASK_CMP_GE | Xtensa::CCMASK_CMP_UO):
      BuildMI(MBB, I, DL, get(Xtensa::BGEU)).addImm(offset).addReg(Cond[2].getReg())
          .addReg(Cond[3].getReg());
      break;
    //synth
    case Xtensa::CCMASK_CMP_GT:
      BuildMI(MBB, I, DL, get(Xtensa::BGT)).addImm(offset).addReg(Cond[2].getReg())
          .addReg(Cond[3].getReg());
      break;
    case Xtensa::CCMASK_CMP_LE:
      BuildMI(MBB, I, DL, get(Xtensa::BLE)).addImm(offset).addReg(Cond[2].getReg())
          .addReg(Cond[3].getReg());
      break;
    case Xtensa::CCMASK_CMP_GT | Xtensa::CCMASK_CMP_UO:
      BuildMI(MBB, I, DL, get(Xtensa::BGTU)).addImm(offset).addReg(Cond[2].getReg())
          .addReg(Cond[3].getReg());
      break;
    case Xtensa::CCMASK_CMP_LE | Xtensa::CCMASK_CMP_UO:
      BuildMI(MBB, I, DL, get(Xtensa::BLEU)).addImm(offset).addReg(Cond[2].getReg())
          .addReg(Cond[3].getReg());
      break;
    default:
      llvm_unreachable("Invalid branch condition code!");
  }
  ++Count;
  return Count;
}

unsigned
XtensaInstrInfo::InsertBranchAtInst(MachineBasicBlock &MBB, MachineBasicBlock::iterator I,
                               MachineBasicBlock *TBB, ArrayRef<MachineOperand> Cond,
                               const DebugLoc &DL) const 
{
  // Shouldn't be a fall through.
  assert(TBB && "InsertBranch must not be told to insert a fallthrough");
  assert(Cond.size() <= 4 &&
         "Xtensa branch conditions have less than four components!");

  if (Cond.empty() || Cond[0].getImm() == Xtensa::CCMASK_ANY) 
  {
    // Unconditional branch
    BuildMI(MBB, I, DL, get(Xtensa::J)).addMBB(TBB);
    return 1;
  }

  // Conditional branch.
  unsigned Count = 0;
  unsigned CC = Cond[0].getImm();
  switch(CC) 
  {
    case Xtensa::CCMASK_CMP_EQ:
      BuildMI(MBB, I, DL, get(Xtensa::BEQ)).addMBB(TBB).addReg(Cond[2].getReg())
          .addReg(Cond[3].getReg());
      break;
    case Xtensa::CCMASK_CMP_NE:
      BuildMI(MBB, I, DL, get(Xtensa::BNE)).addMBB(TBB).addReg(Cond[2].getReg())
          .addReg(Cond[3].getReg());
      break;
    case Xtensa::CCMASK_CMP_LT:
      BuildMI(MBB, I, DL, get(Xtensa::BLT)).addMBB(TBB).addReg(Cond[2].getReg())
          .addReg(Cond[3].getReg());
      break;
    case (Xtensa::CCMASK_CMP_LT | Xtensa::CCMASK_CMP_UO):
      BuildMI(MBB, I, DL, get(Xtensa::BLTU)).addMBB(TBB).addReg(Cond[2].getReg())
          .addReg(Cond[3].getReg());
      break;
    case Xtensa::CCMASK_CMP_GE:
      BuildMI(MBB, I, DL, get(Xtensa::BGE)).addMBB(TBB).addReg(Cond[2].getReg())
          .addReg(Cond[3].getReg());
      break;
    case (Xtensa::CCMASK_CMP_GE | Xtensa::CCMASK_CMP_UO):
      BuildMI(MBB, I, DL, get(Xtensa::BGEU)).addMBB(TBB).addReg(Cond[2].getReg())
          .addReg(Cond[3].getReg());
      break;
    //synth
    case Xtensa::CCMASK_CMP_GT:
      BuildMI(MBB, I, DL, get(Xtensa::BGT)).addMBB(TBB).addReg(Cond[2].getReg())
          .addReg(Cond[3].getReg());
      break;
    case Xtensa::CCMASK_CMP_LE:
      BuildMI(MBB, I, DL, get(Xtensa::BLE)).addMBB(TBB).addReg(Cond[2].getReg())
          .addReg(Cond[3].getReg());
      break;
    case Xtensa::CCMASK_CMP_GT | Xtensa::CCMASK_CMP_UO:
      BuildMI(MBB, I, DL, get(Xtensa::BGTU)).addMBB(TBB).addReg(Cond[2].getReg())
          .addReg(Cond[3].getReg());
      break;
    case Xtensa::CCMASK_CMP_LE | Xtensa::CCMASK_CMP_UO:
      BuildMI(MBB, I, DL, get(Xtensa::BLEU)).addMBB(TBB).addReg(Cond[2].getReg())
          .addReg(Cond[3].getReg());
      break;
    default:
      llvm_unreachable("Invalid branch condition code!");
  }
  ++Count;

  return Count;
}

void
XtensaInstrInfo::copyPhysReg(MachineBasicBlock &MBB,
			      MachineBasicBlock::iterator MBBI, const DebugLoc &DL,
			      unsigned DestReg, unsigned SrcReg,
			      bool KillSrc) const 
{

  unsigned Opcode;
  //when we are copying a phys reg we want the bits for fp
  if (Xtensa::ARRegClass.contains(DestReg, SrcReg))
    Opcode = Xtensa::MOV_N;
  else if (Xtensa::ARRegClass.contains(SrcReg) && Xtensa::SARLRegClass.contains(DestReg))
    Opcode = Xtensa::SSL;
  else if (Xtensa::ARRegClass.contains(SrcReg) && Xtensa::SARRRegClass.contains(DestReg))
    Opcode = Xtensa::SSR;  
  
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
  else  llvm_unreachable("Impossible reg-to-reg copy");

  BuildMI(MBB, MBBI, DL, get(Opcode), DestReg)
    .addReg(SrcReg, getKillRegState(KillSrc));
}

void
XtensaInstrInfo::storeRegToStackSlot(MachineBasicBlock &MBB,
				      MachineBasicBlock::iterator MBBI,
				      unsigned SrcReg, bool isKill,
				      int FrameIdx,
				      const TargetRegisterClass *RC,
				      const TargetRegisterInfo *TRI) const 
{
  DebugLoc DL = MBBI != MBB.end() ? MBBI->getDebugLoc() : DebugLoc();

  // Callers may expect a single instruction, so keep 128-bit moves
  // together for now and lower them after register allocation.
  unsigned LoadOpcode, StoreOpcode;
  getLoadStoreOpcodes(RC, LoadOpcode, StoreOpcode, FrameIdx);
  addFrameReference(BuildMI(MBB, MBBI, DL, get(StoreOpcode))
		    .addReg(SrcReg, getKillRegState(isKill)), FrameIdx);
}

void
XtensaInstrInfo::loadRegFromStackSlot(MachineBasicBlock &MBB,
				       MachineBasicBlock::iterator MBBI,
				       unsigned DestReg, int FrameIdx,
				       const TargetRegisterClass *RC,
				       const TargetRegisterInfo *TRI) const 
{
  DebugLoc DL = MBBI != MBB.end() ? MBBI->getDebugLoc() : DebugLoc();

  // Callers may expect a single instruction, so keep 128-bit moves
  // together for now and lower them after register allocation.
  unsigned LoadOpcode, StoreOpcode;
  getLoadStoreOpcodes(RC, LoadOpcode, StoreOpcode, FrameIdx);
  addFrameReference(BuildMI(MBB, MBBI, DL, get(LoadOpcode), DestReg),
                    FrameIdx);
}

bool XtensaInstrInfo::
reverseBranchCondition(SmallVectorImpl<MachineOperand> &Cond) const 
{
  assert(Cond.size() <= 4 && "Invalid branch condition!");
  //Only need to switch the condition code, not the registers
  switch (Cond[0].getImm()) 
  {
    case Xtensa::CCMASK_CMP_EQ:
      Cond[0].setImm(Xtensa::CCMASK_CMP_NE);
      return false;
    case Xtensa::CCMASK_CMP_NE:
      Cond[0].setImm(Xtensa::CCMASK_CMP_EQ);
      return false;
    case Xtensa::CCMASK_CMP_LT:
      Cond[0].setImm(Xtensa::CCMASK_CMP_GE);
      return false;
    case Xtensa::CCMASK_CMP_GE:
      Cond[0].setImm(Xtensa::CCMASK_CMP_LT);
      return false;
    case Xtensa::CCMASK_CMP_LT | Xtensa::CCMASK_CMP_UO:
      Cond[0].setImm(Xtensa::CCMASK_CMP_GE | Xtensa::CCMASK_CMP_UO);
      return false;
    case Xtensa::CCMASK_CMP_GE | Xtensa::CCMASK_CMP_UO:
      Cond[0].setImm(Xtensa::CCMASK_CMP_LT | Xtensa::CCMASK_CMP_UO);
      return false;
    //synth
    case Xtensa::CCMASK_CMP_GT:
      Cond[0].setImm(Xtensa::CCMASK_CMP_LE);
      return false;
    case Xtensa::CCMASK_CMP_LE:
      Cond[0].setImm(Xtensa::CCMASK_CMP_GT);
      return false;
    case Xtensa::CCMASK_CMP_GT | Xtensa::CCMASK_CMP_UO:
      Cond[0].setImm(Xtensa::CCMASK_CMP_LE | Xtensa::CCMASK_CMP_UO);
      return false;
    case Xtensa::CCMASK_CMP_LE | Xtensa::CCMASK_CMP_UO:
      Cond[0].setImm(Xtensa::CCMASK_CMP_GT | Xtensa::CCMASK_CMP_UO);
      return false;
    default:
      llvm_unreachable("Invalid branch condition!");
  }
}

bool XtensaInstrInfo::isBranch(const MachineBasicBlock::iterator &MI, SmallVectorImpl<MachineOperand> &Cond,
                                const MachineOperand *&Target) const 
{
  switch (MI->getOpcode()) 
  {
    case Xtensa::J:
    case Xtensa::JX:
//    case Xtensa::CALL0:
//    case Xtensa::CALLX0:  
      Cond[0].setImm(Xtensa::CCMASK_ANY);
      Target = &MI->getOperand(0);
      return true;
    case Xtensa::BEQ:
    case Xtensa::BEQI:
    case Xtensa::BEQZ:  
      Cond[0].setImm(Xtensa::CCMASK_CMP_EQ);
      Target = &MI->getOperand(0);
      return true;
    case Xtensa::BNE:
    case Xtensa::BNEI: 
    case Xtensa::BNEZ:  
      Cond[0].setImm(Xtensa::CCMASK_CMP_NE);
      Target = &MI->getOperand(0);
      return true;
    case Xtensa::BLT:
    case Xtensa::BLTI:
    case Xtensa::BLTZ:  
      Cond[0].setImm(Xtensa::CCMASK_CMP_LT);
      Target = &MI->getOperand(0);
      return true;
    case Xtensa::BLTU:
    case Xtensa::BLTUI:  
      Cond[0].setImm(Xtensa::CCMASK_CMP_LT | Xtensa::CCMASK_CMP_UO);
      Target = &MI->getOperand(0);
      return true;
    case Xtensa::BGE:
    case Xtensa::BGEI:
    case Xtensa::BGEZ:  
      Cond[0].setImm(Xtensa::CCMASK_CMP_GE);
      Target = &MI->getOperand(0);
      return true;
    case Xtensa::BGEU:
    case Xtensa::BGEUI:  
      Cond[0].setImm(Xtensa::CCMASK_CMP_GE | Xtensa::CCMASK_CMP_UO);
      Target = &MI->getOperand(0);
      return true;
    case Xtensa::BGT:
      Cond[0].setImm(Xtensa::CCMASK_CMP_GT);
      Target = &MI->getOperand(0);
      return true;
    case Xtensa::BGTU:
      Cond[0].setImm(Xtensa::CCMASK_CMP_GT | Xtensa::CCMASK_CMP_UO);
      Target = &MI->getOperand(0);
      return true;
    case Xtensa::BLE:
      Cond[0].setImm(Xtensa::CCMASK_CMP_LE);
      Target = &MI->getOperand(0);
      return true;
    case Xtensa::BLEU:
      Cond[0].setImm(Xtensa::CCMASK_CMP_LE | Xtensa::CCMASK_CMP_UO);
      Target = &MI->getOperand(0);
      return true;

    default:
      assert(!MI->getDesc().isBranch() && "Unknown branch opcode");
      return false;
  }
}

void XtensaInstrInfo::getLoadStoreOpcodes(const TargetRegisterClass *RC,
                                          unsigned &LoadOpcode, 
                                          unsigned &StoreOpcode,
                                          int64_t offset) const 
{
  if (RC == &Xtensa::ARRegClass)
  {
    if (offset >= 0  &&  offset <= 60)
    {   
      LoadOpcode =  Xtensa::L32I_N;
      StoreOpcode = Xtensa::S32I_N;
    }
    else /* if (offset >= 0  &&  offset <= 1020) */
    {  
      LoadOpcode =  Xtensa::L32I;
      StoreOpcode = Xtensa::S32I;
    }    
  } 
  /* TODO
  else if (RC == &Xtensa::FP32RegClass) 
  {
    LoadOpcode = Xtensa::FLW;
    StoreOpcode = Xtensa::FSW;
  } 
   */ 
  else  llvm_unreachable("Unsupported regclass to load or store");
}

void XtensaInstrInfo::loadImmediate(MachineBasicBlock &MBB,
                                     MachineBasicBlock::iterator MBBI,
                                     unsigned *Reg, int64_t Value) const 
{
  DebugLoc DL = MBBI != MBB.end() ? MBBI->getDebugLoc() : DebugLoc();
  MachineRegisterInfo &RegInfo = MBB.getParent()->getRegInfo();
  const TargetRegisterClass *RC = &Xtensa::ARRegClass;

  //create virtual reg to store immediate
  *Reg = RegInfo.createVirtualRegister(RC);
  if (Value >= 32  &&  Value <= 95)
  {
    BuildMI(MBB, MBBI, DL, get(Xtensa::MOVI_N), *Reg).addImm(Value);
  }
  else if (Value >= -2048  &&  Value <= 2047)
  {
    BuildMI(MBB, MBBI, DL, get(Xtensa::MOVI), *Reg).addImm(Value);
  }  
  else 
  {
    //use L32R to let assembler load immediate best
    //TODO replace to L32R
    BuildMI(MBB, MBBI, DL, get(Xtensa::LI), *Reg).addImm(Value);
  }
}
