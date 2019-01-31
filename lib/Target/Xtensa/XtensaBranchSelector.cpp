//===- XtensaBranchSelector.cpp - Xtensa LLVM Branch Selector Pass --------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "Xtensa.h"
#include "XtensaInstrInfo.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Target/TargetMachine.h"

using namespace llvm;

#define DEBUG_TYPE "xtensa-branch-select"

//STATISTIC(NumExpanded, "Number of branches expanded to long format");

namespace llvm {
void initializeXtensaBSelPass(PassRegistry &);
}

namespace {
struct XtensaBSel : public MachineFunctionPass {
  static char ID;
  XtensaBSel() : MachineFunctionPass(ID) {
    // TODO      initializeXtensaBSelPass(*PassRegistry::getPassRegistry());
  }

  /// BlockSizes - The sizes of the basic blocks in the function.
  std::vector<unsigned> BlockSizes;

  virtual bool runOnMachineFunction(MachineFunction &Fn);

  virtual StringRef getPassName() const { return "Xtensa Branch Selector"; }
};
char XtensaBSel::ID = 0;
} // namespace

INITIALIZE_PASS(XtensaBSel, "xtensa-branch-select", "Xtensa Branch Selector",
                false, false)

/// createXtensaBranchSelectionPass - returns an instance of the Branch
/// Selection Pass
///
FunctionPass *llvm::createXtensaBranchSelectionPass() {
  return new XtensaBSel();
}

bool XtensaBSel::runOnMachineFunction(MachineFunction &Fn) {
  //TODO implement function body
  return true;
}
