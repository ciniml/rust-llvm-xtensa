//#include "Xtensa.h"
#include "MCTargetDesc/XtensaMCTargetDesc.h"
#include "llvm/Support/TargetRegistry.h"

using namespace llvm;

Target llvm::TheXtensaTarget;

extern "C" void LLVMInitializeXtensaTargetInfo() 
{
  RegisterTarget<Triple::xtensa> X(TheXtensaTarget, "xtensa", "Xtensa 32", "XTENSA");
}
