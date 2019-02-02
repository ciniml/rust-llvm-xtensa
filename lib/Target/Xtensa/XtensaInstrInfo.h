//===-- XtensaInstrInfo.h - Xtensa Instruction Information ----------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===--------------------------------------------------------------------------===//
//
// This file contains the Xtensa implementation of the TargetInstrInfo class.
//
//===--------------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_XTENSA_XTENSAINSTRINFO_H
#define LLVM_LIB_TARGET_XTENSA_XTENSAINSTRINFO_H

#include "Xtensa.h"
#include "XtensaRegisterInfo.h"
#include "llvm/CodeGen/TargetInstrInfo.h"
#include "llvm/CodeGen/TargetRegisterInfo.h"

#define GET_INSTRINFO_HEADER

#include "XtensaGenInstrInfo.inc"

namespace llvm {

class XtensaTargetMachine;

namespace XtensaII {
// TODO
enum {
  // Masks out the bits for the access model.
  MO_SYMBOL_MODIFIER = (1 << 0),

  // @GOT (aka @GOTENT)
  MO_GOT = (1 << 0),

  MO_ABS_HI,
  MO_ABS_LO,
  MO_TPREL_HI,
  MO_TPREL_LO
};
} // namespace XtensaII

class XtensaSubtarget;
class XtensaInstrInfo : public XtensaGenInstrInfo {
  const XtensaRegisterInfo RI;
  XtensaSubtarget &STI;

public:
  explicit XtensaInstrInfo(XtensaSubtarget &STI);

  // Override TargetInstrInfo.
  unsigned isLoadFromStackSlot(const MachineInstr &MI,
                               int &FrameIndex) const override;
  unsigned isStoreToStackSlot(const MachineInstr &MI,
                              int &FrameIndex) const override;
  void adjustStackPtr(unsigned SP, int64_t Amount, MachineBasicBlock &MBB,
                      MachineBasicBlock::iterator I) const;
  unsigned GetInstSizeInBytes(MachineInstr *I) const;
  bool analyzeBranch(MachineBasicBlock &MBB, MachineBasicBlock *&TBB,
                     MachineBasicBlock *&FBB,
                     SmallVectorImpl<MachineOperand> &Cond,
                     bool AllowModify) const override;
  unsigned removeBranch(MachineBasicBlock &MBB,
                        int *BytesRemoved = nullptr) const override;
  unsigned insertBranch(MachineBasicBlock &MBB, MachineBasicBlock *TBB,
                        MachineBasicBlock *FBB, ArrayRef<MachineOperand> Cond,
                        const DebugLoc &DL,
                        int *BytesAdded = nullptr) const override;
  unsigned InsertBranchAtInst(MachineBasicBlock &MBB,
                              MachineBasicBlock::iterator I,
                              MachineBasicBlock *TBB,
                              ArrayRef<MachineOperand> Cond,
                              const DebugLoc &DL) const;
  unsigned InsertConstBranchAtInst(MachineBasicBlock &MBB, MachineInstr *I,
                                   int64_t offset,
                                   ArrayRef<MachineOperand> Cond,
                                   DebugLoc DL) const;
  void copyPhysReg(MachineBasicBlock &MBB, MachineBasicBlock::iterator MBBI,
                   const DebugLoc &DL, unsigned DestReg, unsigned SrcReg,
                   bool KillSrc) const override;
  void storeRegToStackSlot(MachineBasicBlock &MBB,
                           MachineBasicBlock::iterator MBBI, unsigned SrcReg,
                           bool isKill, int FrameIndex,
                           const TargetRegisterClass *RC,
                           const TargetRegisterInfo *TRI) const override;
  void loadRegFromStackSlot(MachineBasicBlock &MBB,
                            MachineBasicBlock::iterator MBBI, unsigned DestReg,
                            int FrameIdx, const TargetRegisterClass *RC,
                            const TargetRegisterInfo *TRI) const override;
  bool
  reverseBranchCondition(SmallVectorImpl<MachineOperand> &Cond) const override;

  // Return the XtensaRegisterInfo, which this class owns.
  const XtensaRegisterInfo &getRegisterInfo() const { return RI; }

  // Return true if MI is a conditional or unconditional branch.
  // When returning true, set Cond to the mask of condition-code
  // values on which the instruction will branch, and set Target
  // to the operand that contains the branch target.  This target
  // can be a register or a basic block.
  bool isBranch(const MachineBasicBlock::iterator &MI,
                SmallVectorImpl<MachineOperand> &Cond,
                const MachineOperand *&Target) const;

  // Get the load and store opcodes for a given register class and offset.
  void getLoadStoreOpcodes(const TargetRegisterClass *RC, unsigned &LoadOpcode,
                           unsigned &StoreOpcode, int64_t offset) const;

  // Emit code before MBBI in MI to move immediate value Value into
  // physical register Reg.
  void loadImmediate(MachineBasicBlock &MBB, MachineBasicBlock::iterator MBBI,
                     unsigned *Reg, int64_t Value) const;

  unsigned BranchType(unsigned BR_CODE) const;
};
} // end namespace llvm

#endif /* LLVM_LIB_TARGET_XTENSA_XTENSAINSTRINFO_H */
