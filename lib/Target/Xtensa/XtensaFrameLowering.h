#ifndef LLVM_LIB_TARGET_XTENSA_XTENSAFRAMELOWERING_H
#define LLVM_LIB_TARGET_XTENSA_XTENSAFRAMELOWERING_H

//#include "llvm/Target/TargetFrameLowering.h"
#include "llvm/CodeGen/TargetFrameLowering.h"

namespace llvm {
class XtensaTargetMachine;
class XtensaSubtarget;

class XtensaFrameLowering : public TargetFrameLowering 
{
public:
  XtensaFrameLowering();

  bool hasFP(const MachineFunction &MF) const;

  /// emitProlog/emitEpilog - These methods insert prolog and epilog code into
  /// the function.
  void emitPrologue(MachineFunction&, MachineBasicBlock&) const;
  void emitEpilogue(MachineFunction &MF, MachineBasicBlock &MBB) const;

  MachineBasicBlock::iterator
  eliminateCallFramePseudoInstr(MachineFunction &MF,
                                     MachineBasicBlock &MBB,
                                     MachineBasicBlock::iterator I) const;

  bool spillCalleeSavedRegisters(MachineBasicBlock &MBB,
                                 MachineBasicBlock::iterator MI,
                                 const std::vector<CalleeSavedInfo> &CSI,
                                 const TargetRegisterInfo *TRI) const;

  bool hasReservedCallFrame(const MachineFunction &MF) const;

  void determineCalleeSaves(MachineFunction &MF, BitVector &SavedRegs,
                                            RegScavenger *RS) const override;
};

} // End llvm namespace

#endif /* LLVM_LIB_TARGET_XTENSA_XTENSAFRAMELOWERING_H */

