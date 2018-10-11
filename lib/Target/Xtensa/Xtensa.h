#ifndef LLVM_LIB_TARGET_XTENSA_XTENSA_H
#define LLVM_LIB_TARGET_XTENSA_XTENSA_H

#include "MCTargetDesc/XtensaMCTargetDesc.h"
#include "llvm/PassRegistry.h"
#include "llvm/Support/CodeGen.h"

namespace llvm 
{
  class XtensaTargetMachine;
  class Pass;
  class FunctionPass;

  namespace Xtensa 
  {
  // Branch type
  const unsigned UBRANCH = 1;
  const unsigned CBRANCH_RR = 2;
  const unsigned CBRANCH_RI = 3;
  const unsigned CBRANCH_RZ = 4;
  const unsigned CBRANCH_B = 5;
  } // end namespace Xtensa
    
  FunctionPass *createXtensaISelDag(XtensaTargetMachine &TM,
                                     CodeGenOpt::Level OptLevel);
  FunctionPass *createXtensaBranchSelectionPass();
  FunctionPass *createXtensaSizeReductionPass();

  void initializeXtensaZOLPassPass(PassRegistry &Registry);
  Pass *createXtensaZOLPass();
  } // end namespace llvm;
#endif /* LLVM_LIB_TARGET_XTENSA_XTENSA_H */
  
