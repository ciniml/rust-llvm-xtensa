#ifndef LLVM_LIB_TARGET_XTENSA_XTENSAMCFIXUPS_H
#define LLVM_LIB_TARGET_XTENSA_XTENSAMCFIXUPS_H

#include "llvm/MC/MCFixup.h"

namespace llvm 
{
namespace Xtensa 
{
  enum FixupKind 
  {
    // These correspond directly to Xtensa relocations.
    fixup_xtensa_brlo = FirstTargetFixupKind,
    fixup_xtensa_brhi,
    fixup_xtensa_jal,
    fixup_xtensa_call,
    fixup_xtensa_call_plt,

    // Marker
    LastTargetFixupKind,
    NumTargetFixupKinds = LastTargetFixupKind - FirstTargetFixupKind
  };
} // end namespace Xtensa
} // end namespace llvm

#endif /* LLVM_LIB_TARGET_XTENSA_XTENSAMCFIXUPS_H */

