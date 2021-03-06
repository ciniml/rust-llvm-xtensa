//===-- AArch64BaseInfo.h - Top level definitions for AArch64- --*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains small standalone helper functions and enum definitions for
// the AArch64 target useful for the compiler back-end and the MC libraries.
// As such, it deliberately does not include references to LLVM core
// code gen types, passes, etc..
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_AARCH64_BASEINFO_H
#define LLVM_AARCH64_BASEINFO_H

#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/ErrorHandling.h"

namespace llvm {

// // Enums corresponding to AArch64 condition codes
namespace A64CC {
  // The CondCodes constants map directly to the 4-bit encoding of the
  // condition field for predicated instructions.
  enum CondCodes {   // Meaning (integer)          Meaning (floating-point)
    EQ = 0,        // Equal                      Equal
    NE,            // Not equal                  Not equal, or unordered
    HS,            // Unsigned higher or same    >, ==, or unordered
    LO,            // Unsigned lower or same     Less than
    MI,            // Minus, negative            Less than
    PL,            // Plus, positive or zero     >, ==, or unordered
    VS,            // Overflow                   Unordered
    VC,            // No overflow                Ordered
    HI,            // Unsigned higher            Greater than, or unordered
    LS,            // Unsigned lower or same     Less than or equal
    GE,            // Greater than or equal      Greater than or equal
    LT,            // Less than                  Less than, or unordered
    GT,            // Signed greater than        Greater than
    LE,            // Signed less than or equal  <, ==, or unordered
    AL,            // Always (unconditional)     Always (unconditional)
    NV,             // Always (unconditional)     Always (unconditional)
    // Note the NV exists purely to disassemble 0b1111. Execution
    // is "always".
    Invalid
  };

} // namespace A64CC

inline static const char *A64CondCodeToString(A64CC::CondCodes CC) {
  switch (CC) {
  default: llvm_unreachable("Unknown condition code");
  case A64CC::EQ:  return "eq";
  case A64CC::NE:  return "ne";
  case A64CC::HS:  return "hs";
  case A64CC::LO:  return "lo";
  case A64CC::MI:  return "mi";
  case A64CC::PL:  return "pl";
  case A64CC::VS:  return "vs";
  case A64CC::VC:  return "vc";
  case A64CC::HI:  return "hi";
  case A64CC::LS:  return "ls";
  case A64CC::GE:  return "ge";
  case A64CC::LT:  return "lt";
  case A64CC::GT:  return "gt";
  case A64CC::LE:  return "le";
  case A64CC::AL:  return "al";
  case A64CC::NV:  return "nv";
  }
}

inline static A64CC::CondCodes A64StringToCondCode(StringRef CondStr) {
  return StringSwitch<A64CC::CondCodes>(CondStr.lower())
             .Case("eq", A64CC::EQ)
             .Case("ne", A64CC::NE)
             .Case("ne", A64CC::NE)
             .Case("hs", A64CC::HS)
             .Case("cs", A64CC::HS)
             .Case("lo", A64CC::LO)
             .Case("cc", A64CC::LO)
             .Case("mi", A64CC::MI)
             .Case("pl", A64CC::PL)
             .Case("vs", A64CC::VS)
             .Case("vc", A64CC::VC)
             .Case("hi", A64CC::HI)
             .Case("ls", A64CC::LS)
             .Case("ge", A64CC::GE)
             .Case("lt", A64CC::LT)
             .Case("gt", A64CC::GT)
             .Case("le", A64CC::LE)
             .Case("al", A64CC::AL)
             .Case("nv", A64CC::NV)
             .Default(A64CC::Invalid);
}

inline static A64CC::CondCodes A64InvertCondCode(A64CC::CondCodes CC) {
  // It turns out that the condition codes have been designed so that in order
  // to reverse the intent of the condition you only have to invert the low bit:

  return static_cast<A64CC::CondCodes>(static_cast<unsigned>(CC) ^ 0x1);
}

/// Instances of this class can perform bidirectional mapping from random
/// identifier strings to operand encodings. For example "MSR" takes a named
/// system-register which must be encoded somehow and decoded for printing. This
/// central location means that the information for those transformations is not
/// duplicated and remains in sync.
///
/// FIXME: currently the algorithm is a completely unoptimised linear
/// search. Obviously this could be improved, but we would probably want to work
/// out just how often these instructions are emitted before working on it. It
/// might even be optimal to just reorder the tables for the common instructions
/// rather than changing the algorithm.
struct NamedImmMapper {
  struct Mapping {
    const char *Name;
    uint32_t Value;
  };

  template<int N>
  NamedImmMapper(const Mapping (&Pairs)[N], uint32_t TooBigImm)
    : Pairs(&Pairs[0]), NumPairs(N), TooBigImm(TooBigImm) {}

  StringRef toString(uint32_t Value, bool &Valid) const;
  uint32_t fromString(StringRef Name, bool &Valid) const;

  /// Many of the instructions allow an alternative assembly form consisting of
  /// a simple immediate. Currently the only valid forms are ranges [0, N) where
  /// N being 0 indicates no immediate syntax-form is allowed.
  bool validImm(uint32_t Value) const;
protected:
  const Mapping *Pairs;
  size_t NumPairs;
  uint32_t TooBigImm;
};

namespace A64AT {
  enum ATValues {
    Invalid = -1,    // Op0 Op1  CRn   CRm   Op2
    S1E1R = 0x43c0,  // 01  000  0111  1000  000
    S1E2R = 0x63c0,  // 01  100  0111  1000  000
    S1E3R = 0x73c0,  // 01  110  0111  1000  000
    S1E1W = 0x43c1,  // 01  000  0111  1000  001
    S1E2W = 0x63c1,  // 01  100  0111  1000  001
    S1E3W = 0x73c1,  // 01  110  0111  1000  001
    S1E0R = 0x43c2,  // 01  000  0111  1000  010
    S1E0W = 0x43c3,  // 01  000  0111  1000  011
    S12E1R = 0x63c4, // 01  100  0111  1000  100
    S12E1W = 0x63c5, // 01  100  0111  1000  101
    S12E0R = 0x63c6, // 01  100  0111  1000  110
    S12E0W = 0x63c7  // 01  100  0111  1000  111
  };

  struct ATMapper : NamedImmMapper {
    const static Mapping ATPairs[];

    ATMapper();
  };

}
namespace A64DB {
  enum DBValues {
    Invalid = -1,
    OSHLD = 0x1,
    OSHST = 0x2,
    OSH =   0x3,
    NSHLD = 0x5,
    NSHST = 0x6,
    NSH =   0x7,
    ISHLD = 0x9,
    ISHST = 0xa,
    ISH =   0xb,
    LD =    0xd,
    ST =    0xe,
    SY =    0xf
  };

  struct DBarrierMapper : NamedImmMapper {
    const static Mapping DBarrierPairs[];

    DBarrierMapper();
  };
}

namespace  A64DC {
  enum DCValues {
    Invalid = -1,   // Op1  CRn   CRm   Op2
    ZVA   = 0x5ba1, // 01  011  0111  0100  001
    IVAC  = 0x43b1, // 01  000  0111  0110  001
    ISW   = 0x43b2, // 01  000  0111  0110  010
    CVAC  = 0x5bd1, // 01  011  0111  1010  001
    CSW   = 0x43d2, // 01  000  0111  1010  010
    CVAU  = 0x5bd9, // 01  011  0111  1011  001
    CIVAC = 0x5bf1, // 01  011  0111  1110  001
    CISW  = 0x43f2  // 01  000  0111  1110  010
  };

  struct DCMapper : NamedImmMapper {
    const static Mapping DCPairs[];

    DCMapper();
  };

}

namespace  A64IC {
  enum ICValues {
    Invalid = -1,     // Op1  CRn   CRm   Op2
    IALLUIS = 0x0388, // 000  0111  0001  000
    IALLU = 0x03a8,   // 000  0111  0101  000
    IVAU = 0x1ba9     // 011  0111  0101  001
  };


  struct ICMapper : NamedImmMapper {
    const static Mapping ICPairs[];

    ICMapper();
  };

  static inline bool NeedsRegister(ICValues Val) {
    return Val == IVAU;
  }
}

namespace  A64ISB {
  enum ISBValues {
    Invalid = -1,
    SY = 0xf
  };
  struct ISBMapper : NamedImmMapper {
    const static Mapping ISBPairs[];

    ISBMapper();
  };
}

namespace A64PRFM {
  enum PRFMValues {
    Invalid = -1,
    PLDL1KEEP = 0x00,
    PLDL1STRM = 0x01,
    PLDL2KEEP = 0x02,
    PLDL2STRM = 0x03,
    PLDL3KEEP = 0x04,
    PLDL3STRM = 0x05,
    PLIL1KEEP = 0x08,
    PLIL1STRM = 0x09,
    PLIL2KEEP = 0x0a,
    PLIL2STRM = 0x0b,
    PLIL3KEEP = 0x0c,
    PLIL3STRM = 0x0d,
    PSTL1KEEP = 0x10,
    PSTL1STRM = 0x11,
    PSTL2KEEP = 0x12,
    PSTL2STRM = 0x13,
    PSTL3KEEP = 0x14,
    PSTL3STRM = 0x15
  };

  struct PRFMMapper : NamedImmMapper {
    const static Mapping PRFMPairs[];

    PRFMMapper();
  };
}

namespace A64PState {
  enum PStateValues {
    Invalid = -1,
    SPSel = 0x05,
    DAIFSet = 0x1e,
    DAIFClr = 0x1f
  };

  struct PStateMapper : NamedImmMapper {
    const static Mapping PStatePairs[];

    PStateMapper();
  };

}

namespace A64SE {
    enum ShiftExtSpecifiers {
        Invalid = -1,
        LSL,
        LSR,
        ASR,
        ROR,

        UXTB,
        UXTH,
        UXTW,
        UXTX,

        SXTB,
        SXTH,
        SXTW,
        SXTX
    };
}

namespace A64SysReg {
  enum SysRegROValues {
    MDCCSR_EL0        = 0x9808, // 10  011  0000  0001  000
    DBGDTRRX_EL0      = 0x9828, // 10  011  0000  0101  000
    MDRAR_EL1         = 0x8080, // 10  000  0001  0000  000
    OSLSR_EL1         = 0x808c, // 10  000  0001  0001  100
    DBGAUTHSTATUS_EL1 = 0x83f6, // 10  000  0111  1110  110
    PMCEID0_EL0       = 0xdce6, // 11  011  1001  1100  110
    PMCEID1_EL0       = 0xdce7, // 11  011  1001  1100  111
    MIDR_EL1          = 0xc000, // 11  000  0000  0000  000
    CCSIDR_EL1        = 0xc800, // 11  001  0000  0000  000
    CLIDR_EL1         = 0xc801, // 11  001  0000  0000  001
    CTR_EL0           = 0xd801, // 11  011  0000  0000  001
    MPIDR_EL1         = 0xc005, // 11  000  0000  0000  101
    REVIDR_EL1        = 0xc006, // 11  000  0000  0000  110
    AIDR_EL1          = 0xc807, // 11  001  0000  0000  111
    DCZID_EL0         = 0xd807, // 11  011  0000  0000  111
    ID_PFR0_EL1       = 0xc008, // 11  000  0000  0001  000
    ID_PFR1_EL1       = 0xc009, // 11  000  0000  0001  001
    ID_DFR0_EL1       = 0xc00a, // 11  000  0000  0001  010
    ID_AFR0_EL1       = 0xc00b, // 11  000  0000  0001  011
    ID_MMFR0_EL1      = 0xc00c, // 11  000  0000  0001  100
    ID_MMFR1_EL1      = 0xc00d, // 11  000  0000  0001  101
    ID_MMFR2_EL1      = 0xc00e, // 11  000  0000  0001  110
    ID_MMFR3_EL1      = 0xc00f, // 11  000  0000  0001  111
    ID_ISAR0_EL1      = 0xc010, // 11  000  0000  0010  000
    ID_ISAR1_EL1      = 0xc011, // 11  000  0000  0010  001
    ID_ISAR2_EL1      = 0xc012, // 11  000  0000  0010  010
    ID_ISAR3_EL1      = 0xc013, // 11  000  0000  0010  011
    ID_ISAR4_EL1      = 0xc014, // 11  000  0000  0010  100
    ID_ISAR5_EL1      = 0xc015, // 11  000  0000  0010  101
    ID_AA64PFR0_EL1   = 0xc020, // 11  000  0000  0100  000
    ID_AA64PFR1_EL1   = 0xc021, // 11  000  0000  0100  001
    ID_AA64DFR0_EL1   = 0xc028, // 11  000  0000  0101  000
    ID_AA64DFR1_EL1   = 0xc029, // 11  000  0000  0101  001
    ID_AA64AFR0_EL1   = 0xc02c, // 11  000  0000  0101  100
    ID_AA64AFR1_EL1   = 0xc02d, // 11  000  0000  0101  101
    ID_AA64ISAR0_EL1  = 0xc030, // 11  000  0000  0110  000
    ID_AA64ISAR1_EL1  = 0xc031, // 11  000  0000  0110  001
    ID_AA64MMFR0_EL1  = 0xc038, // 11  000  0000  0111  000
    ID_AA64MMFR1_EL1  = 0xc039, // 11  000  0000  0111  001
    MVFR0_EL1         = 0xc018, // 11  000  0000  0011  000
    MVFR1_EL1         = 0xc019, // 11  000  0000  0011  001
    MVFR2_EL1         = 0xc01a, // 11  000  0000  0011  010
    RVBAR_EL1         = 0xc601, // 11  000  1100  0000  001
    RVBAR_EL2         = 0xe601, // 11  100  1100  0000  001
    RVBAR_EL3         = 0xf601, // 11  110  1100  0000  001
    ISR_EL1           = 0xc608, // 11  000  1100  0001  000
    CNTPCT_EL0        = 0xdf01, // 11  011  1110  0000  001
    CNTVCT_EL0        = 0xdf02  // 11  011  1110  0000  010
  };

  enum SysRegWOValues {
    DBGDTRTX_EL0      = 0x9828, // 10  011  0000  0101  000
    OSLAR_EL1         = 0x8084, // 10  000  0001  0000  100
    PMSWINC_EL0       = 0xdce4  // 11  011  1001  1100  100
  };

  enum SysRegValues {
    Invalid = -1,               // Op0 Op1  CRn   CRm   Op2
    OSDTRRX_EL1       = 0x8002, // 10  000  0000  0000  010
    OSDTRTX_EL1       = 0x801a, // 10  000  0000  0011  010
    TEECR32_EL1       = 0x9000, // 10  010  0000  0000  000
    MDCCINT_EL1       = 0x8010, // 10  000  0000  0010  000
    MDSCR_EL1         = 0x8012, // 10  000  0000  0010  010
    DBGDTR_EL0        = 0x9820, // 10  011  0000  0100  000
    OSECCR_EL1        = 0x8032, // 10  000  0000  0110  010
    DBGVCR32_EL2      = 0xa038, // 10  100  0000  0111  000
    DBGBVR0_EL1       = 0x8004, // 10  000  0000  0000  100
    DBGBVR1_EL1       = 0x800c, // 10  000  0000  0001  100
    DBGBVR2_EL1       = 0x8014, // 10  000  0000  0010  100
    DBGBVR3_EL1       = 0x801c, // 10  000  0000  0011  100
    DBGBVR4_EL1       = 0x8024, // 10  000  0000  0100  100
    DBGBVR5_EL1       = 0x802c, // 10  000  0000  0101  100
    DBGBVR6_EL1       = 0x8034, // 10  000  0000  0110  100
    DBGBVR7_EL1       = 0x803c, // 10  000  0000  0111  100
    DBGBVR8_EL1       = 0x8044, // 10  000  0000  1000  100
    DBGBVR9_EL1       = 0x804c, // 10  000  0000  1001  100
    DBGBVR10_EL1      = 0x8054, // 10  000  0000  1010  100
    DBGBVR11_EL1      = 0x805c, // 10  000  0000  1011  100
    DBGBVR12_EL1      = 0x8064, // 10  000  0000  1100  100
    DBGBVR13_EL1      = 0x806c, // 10  000  0000  1101  100
    DBGBVR14_EL1      = 0x8074, // 10  000  0000  1110  100
    DBGBVR15_EL1      = 0x807c, // 10  000  0000  1111  100
    DBGBCR0_EL1       = 0x8005, // 10  000  0000  0000  101
    DBGBCR1_EL1       = 0x800d, // 10  000  0000  0001  101
    DBGBCR2_EL1       = 0x8015, // 10  000  0000  0010  101
    DBGBCR3_EL1       = 0x801d, // 10  000  0000  0011  101
    DBGBCR4_EL1       = 0x8025, // 10  000  0000  0100  101
    DBGBCR5_EL1       = 0x802d, // 10  000  0000  0101  101
    DBGBCR6_EL1       = 0x8035, // 10  000  0000  0110  101
    DBGBCR7_EL1       = 0x803d, // 10  000  0000  0111  101
    DBGBCR8_EL1       = 0x8045, // 10  000  0000  1000  101
    DBGBCR9_EL1       = 0x804d, // 10  000  0000  1001  101
    DBGBCR10_EL1      = 0x8055, // 10  000  0000  1010  101
    DBGBCR11_EL1      = 0x805d, // 10  000  0000  1011  101
    DBGBCR12_EL1      = 0x8065, // 10  000  0000  1100  101
    DBGBCR13_EL1      = 0x806d, // 10  000  0000  1101  101
    DBGBCR14_EL1      = 0x8075, // 10  000  0000  1110  101
    DBGBCR15_EL1      = 0x807d, // 10  000  0000  1111  101
    DBGWVR0_EL1       = 0x8006, // 10  000  0000  0000  110
    DBGWVR1_EL1       = 0x800e, // 10  000  0000  0001  110
    DBGWVR2_EL1       = 0x8016, // 10  000  0000  0010  110
    DBGWVR3_EL1       = 0x801e, // 10  000  0000  0011  110
    DBGWVR4_EL1       = 0x8026, // 10  000  0000  0100  110
    DBGWVR5_EL1       = 0x802e, // 10  000  0000  0101  110
    DBGWVR6_EL1       = 0x8036, // 10  000  0000  0110  110
    DBGWVR7_EL1       = 0x803e, // 10  000  0000  0111  110
    DBGWVR8_EL1       = 0x8046, // 10  000  0000  1000  110
    DBGWVR9_EL1       = 0x804e, // 10  000  0000  1001  110
    DBGWVR10_EL1      = 0x8056, // 10  000  0000  1010  110
    DBGWVR11_EL1      = 0x805e, // 10  000  0000  1011  110
    DBGWVR12_EL1      = 0x8066, // 10  000  0000  1100  110
    DBGWVR13_EL1      = 0x806e, // 10  000  0000  1101  110
    DBGWVR14_EL1      = 0x8076, // 10  000  0000  1110  110
    DBGWVR15_EL1      = 0x807e, // 10  000  0000  1111  110
    DBGWCR0_EL1       = 0x8007, // 10  000  0000  0000  111
    DBGWCR1_EL1       = 0x800f, // 10  000  0000  0001  111
    DBGWCR2_EL1       = 0x8017, // 10  000  0000  0010  111
    DBGWCR3_EL1       = 0x801f, // 10  000  0000  0011  111
    DBGWCR4_EL1       = 0x8027, // 10  000  0000  0100  111
    DBGWCR5_EL1       = 0x802f, // 10  000  0000  0101  111
    DBGWCR6_EL1       = 0x8037, // 10  000  0000  0110  111
    DBGWCR7_EL1       = 0x803f, // 10  000  0000  0111  111
    DBGWCR8_EL1       = 0x8047, // 10  000  0000  1000  111
    DBGWCR9_EL1       = 0x804f, // 10  000  0000  1001  111
    DBGWCR10_EL1      = 0x8057, // 10  000  0000  1010  111
    DBGWCR11_EL1      = 0x805f, // 10  000  0000  1011  111
    DBGWCR12_EL1      = 0x8067, // 10  000  0000  1100  111
    DBGWCR13_EL1      = 0x806f, // 10  000  0000  1101  111
    DBGWCR14_EL1      = 0x8077, // 10  000  0000  1110  111
    DBGWCR15_EL1      = 0x807f, // 10  000  0000  1111  111
    TEEHBR32_EL1      = 0x9080, // 10  010  0001  0000  000
    OSDLR_EL1         = 0x809c, // 10  000  0001  0011  100
    DBGPRCR_EL1       = 0x80a4, // 10  000  0001  0100  100
    DBGCLAIMSET_EL1   = 0x83c6, // 10  000  0111  1000  110
    DBGCLAIMCLR_EL1   = 0x83ce, // 10  000  0111  1001  110
    CSSELR_EL1        = 0xd000, // 11  010  0000  0000  000
    VPIDR_EL2         = 0xe000, // 11  100  0000  0000  000
    VMPIDR_EL2        = 0xe005, // 11  100  0000  0000  101
    CPACR_EL1         = 0xc082, // 11  000  0001  0000  010
    SCTLR_EL1         = 0xc080, // 11  000  0001  0000  000
    SCTLR_EL2         = 0xe080, // 11  100  0001  0000  000
    SCTLR_EL3         = 0xf080, // 11  110  0001  0000  000
    ACTLR_EL1         = 0xc081, // 11  000  0001  0000  001
    ACTLR_EL2         = 0xe081, // 11  100  0001  0000  001
    ACTLR_EL3         = 0xf081, // 11  110  0001  0000  001
    HCR_EL2           = 0xe088, // 11  100  0001  0001  000
    SCR_EL3           = 0xf088, // 11  110  0001  0001  000
    MDCR_EL2          = 0xe089, // 11  100  0001  0001  001
    SDER32_EL3        = 0xf089, // 11  110  0001  0001  001
    CPTR_EL2          = 0xe08a, // 11  100  0001  0001  010
    CPTR_EL3          = 0xf08a, // 11  110  0001  0001  010
    HSTR_EL2          = 0xe08b, // 11  100  0001  0001  011
    HACR_EL2          = 0xe08f, // 11  100  0001  0001  111
    MDCR_EL3          = 0xf099, // 11  110  0001  0011  001
    TTBR0_EL1         = 0xc100, // 11  000  0010  0000  000
    TTBR0_EL2         = 0xe100, // 11  100  0010  0000  000
    TTBR0_EL3         = 0xf100, // 11  110  0010  0000  000
    TTBR1_EL1         = 0xc101, // 11  000  0010  0000  001
    TCR_EL1           = 0xc102, // 11  000  0010  0000  010
    TCR_EL2           = 0xe102, // 11  100  0010  0000  010
    TCR_EL3           = 0xf102, // 11  110  0010  0000  010
    VTTBR_EL2         = 0xe108, // 11  100  0010  0001  000
    VTCR_EL2          = 0xe10a, // 11  100  0010  0001  010
    DACR32_EL2        = 0xe180, // 11  100  0011  0000  000
    SPSR_EL1          = 0xc200, // 11  000  0100  0000  000
    SPSR_EL2          = 0xe200, // 11  100  0100  0000  000
    SPSR_EL3          = 0xf200, // 11  110  0100  0000  000
    ELR_EL1           = 0xc201, // 11  000  0100  0000  001
    ELR_EL2           = 0xe201, // 11  100  0100  0000  001
    ELR_EL3           = 0xf201, // 11  110  0100  0000  001
    SP_EL0            = 0xc208, // 11  000  0100  0001  000
    SP_EL1            = 0xe208, // 11  100  0100  0001  000
    SP_EL2            = 0xf208, // 11  110  0100  0001  000
    SPSel             = 0xc210, // 11  000  0100  0010  000
    NZCV              = 0xda10, // 11  011  0100  0010  000
    DAIF              = 0xda11, // 11  011  0100  0010  001
    CurrentEL         = 0xc212, // 11  000  0100  0010  010
    SPSR_irq          = 0xe218, // 11  100  0100  0011  000
    SPSR_abt          = 0xe219, // 11  100  0100  0011  001
    SPSR_und          = 0xe21a, // 11  100  0100  0011  010
    SPSR_fiq          = 0xe21b, // 11  100  0100  0011  011
    FPCR              = 0xda20, // 11  011  0100  0100  000
    FPSR              = 0xda21, // 11  011  0100  0100  001
    DSPSR_EL0         = 0xda28, // 11  011  0100  0101  000
    DLR_EL0           = 0xda29, // 11  011  0100  0101  001
    IFSR32_EL2        = 0xe281, // 11  100  0101  0000  001
    AFSR0_EL1         = 0xc288, // 11  000  0101  0001  000
    AFSR0_EL2         = 0xe288, // 11  100  0101  0001  000
    AFSR0_EL3         = 0xf288, // 11  110  0101  0001  000
    AFSR1_EL1         = 0xc289, // 11  000  0101  0001  001
    AFSR1_EL2         = 0xe289, // 11  100  0101  0001  001
    AFSR1_EL3         = 0xf289, // 11  110  0101  0001  001
    ESR_EL1           = 0xc290, // 11  000  0101  0010  000
    ESR_EL2           = 0xe290, // 11  100  0101  0010  000
    ESR_EL3           = 0xf290, // 11  110  0101  0010  000
    FPEXC32_EL2       = 0xe298, // 11  100  0101  0011  000
    FAR_EL1           = 0xc300, // 11  000  0110  0000  000
    FAR_EL2           = 0xe300, // 11  100  0110  0000  000
    FAR_EL3           = 0xf300, // 11  110  0110  0000  000
    HPFAR_EL2         = 0xe304, // 11  100  0110  0000  100
    PAR_EL1           = 0xc3a0, // 11  000  0111  0100  000
    PMCR_EL0          = 0xdce0, // 11  011  1001  1100  000
    PMCNTENSET_EL0    = 0xdce1, // 11  011  1001  1100  001
    PMCNTENCLR_EL0    = 0xdce2, // 11  011  1001  1100  010
    PMOVSCLR_EL0      = 0xdce3, // 11  011  1001  1100  011
    PMSELR_EL0        = 0xdce5, // 11  011  1001  1100  101
    PMCCNTR_EL0       = 0xdce8, // 11  011  1001  1101  000
    PMXEVTYPER_EL0    = 0xdce9, // 11  011  1001  1101  001
    PMXEVCNTR_EL0     = 0xdcea, // 11  011  1001  1101  010
    PMUSERENR_EL0     = 0xdcf0, // 11  011  1001  1110  000
    PMINTENSET_EL1    = 0xc4f1, // 11  000  1001  1110  001
    PMINTENCLR_EL1    = 0xc4f2, // 11  000  1001  1110  010
    PMOVSSET_EL0      = 0xdcf3, // 11  011  1001  1110  011
    MAIR_EL1          = 0xc510, // 11  000  1010  0010  000
    MAIR_EL2          = 0xe510, // 11  100  1010  0010  000
    MAIR_EL3          = 0xf510, // 11  110  1010  0010  000
    AMAIR_EL1         = 0xc518, // 11  000  1010  0011  000
    AMAIR_EL2         = 0xe518, // 11  100  1010  0011  000
    AMAIR_EL3         = 0xf518, // 11  110  1010  0011  000
    VBAR_EL1          = 0xc600, // 11  000  1100  0000  000
    VBAR_EL2          = 0xe600, // 11  100  1100  0000  000
    VBAR_EL3          = 0xf600, // 11  110  1100  0000  000
    RMR_EL1           = 0xc602, // 11  000  1100  0000  010
    RMR_EL2           = 0xe602, // 11  100  1100  0000  010
    RMR_EL3           = 0xf602, // 11  110  1100  0000  010
    CONTEXTIDR_EL1    = 0xc681, // 11  000  1101  0000  001
    TPIDR_EL0         = 0xde82, // 11  011  1101  0000  010
    TPIDR_EL2         = 0xe682, // 11  100  1101  0000  010
    TPIDR_EL3         = 0xf682, // 11  110  1101  0000  010
    TPIDRRO_EL0       = 0xde83, // 11  011  1101  0000  011
    TPIDR_EL1         = 0xc684, // 11  000  1101  0000  100
    CNTFRQ_EL0        = 0xdf00, // 11  011  1110  0000  000
    CNTVOFF_EL2       = 0xe703, // 11  100  1110  0000  011
    CNTKCTL_EL1       = 0xc708, // 11  000  1110  0001  000
    CNTHCTL_EL2       = 0xe708, // 11  100  1110  0001  000
    CNTP_TVAL_EL0     = 0xdf10, // 11  011  1110  0010  000
    CNTHP_TVAL_EL2    = 0xe710, // 11  100  1110  0010  000
    CNTPS_TVAL_EL1    = 0xff10, // 11  111  1110  0010  000
    CNTP_CTL_EL0      = 0xdf11, // 11  011  1110  0010  001
    CNTHP_CTL_EL2     = 0xe711, // 11  100  1110  0010  001
    CNTPS_CTL_EL1     = 0xff11, // 11  111  1110  0010  001
    CNTP_CVAL_EL0     = 0xdf12, // 11  011  1110  0010  010
    CNTHP_CVAL_EL2    = 0xe712, // 11  100  1110  0010  010
    CNTPS_CVAL_EL1    = 0xff12, // 11  111  1110  0010  010
    CNTV_TVAL_EL0     = 0xdf18, // 11  011  1110  0011  000
    CNTV_CTL_EL0      = 0xdf19, // 11  011  1110  0011  001
    CNTV_CVAL_EL0     = 0xdf1a, // 11  011  1110  0011  010
    PMEVCNTR0_EL0     = 0xdf40, // 11  011  1110  1000  000
    PMEVCNTR1_EL0     = 0xdf41, // 11  011  1110  1000  001
    PMEVCNTR2_EL0     = 0xdf42, // 11  011  1110  1000  010
    PMEVCNTR3_EL0     = 0xdf43, // 11  011  1110  1000  011
    PMEVCNTR4_EL0     = 0xdf44, // 11  011  1110  1000  100
    PMEVCNTR5_EL0     = 0xdf45, // 11  011  1110  1000  101
    PMEVCNTR6_EL0     = 0xdf46, // 11  011  1110  1000  110
    PMEVCNTR7_EL0     = 0xdf47, // 11  011  1110  1000  111
    PMEVCNTR8_EL0     = 0xdf48, // 11  011  1110  1001  000
    PMEVCNTR9_EL0     = 0xdf49, // 11  011  1110  1001  001
    PMEVCNTR10_EL0    = 0xdf4a, // 11  011  1110  1001  010
    PMEVCNTR11_EL0    = 0xdf4b, // 11  011  1110  1001  011
    PMEVCNTR12_EL0    = 0xdf4c, // 11  011  1110  1001  100
    PMEVCNTR13_EL0    = 0xdf4d, // 11  011  1110  1001  101
    PMEVCNTR14_EL0    = 0xdf4e, // 11  011  1110  1001  110
    PMEVCNTR15_EL0    = 0xdf4f, // 11  011  1110  1001  111
    PMEVCNTR16_EL0    = 0xdf50, // 11  011  1110  1010  000
    PMEVCNTR17_EL0    = 0xdf51, // 11  011  1110  1010  001
    PMEVCNTR18_EL0    = 0xdf52, // 11  011  1110  1010  010
    PMEVCNTR19_EL0    = 0xdf53, // 11  011  1110  1010  011
    PMEVCNTR20_EL0    = 0xdf54, // 11  011  1110  1010  100
    PMEVCNTR21_EL0    = 0xdf55, // 11  011  1110  1010  101
    PMEVCNTR22_EL0    = 0xdf56, // 11  011  1110  1010  110
    PMEVCNTR23_EL0    = 0xdf57, // 11  011  1110  1010  111
    PMEVCNTR24_EL0    = 0xdf58, // 11  011  1110  1011  000
    PMEVCNTR25_EL0    = 0xdf59, // 11  011  1110  1011  001
    PMEVCNTR26_EL0    = 0xdf5a, // 11  011  1110  1011  010
    PMEVCNTR27_EL0    = 0xdf5b, // 11  011  1110  1011  011
    PMEVCNTR28_EL0    = 0xdf5c, // 11  011  1110  1011  100
    PMEVCNTR29_EL0    = 0xdf5d, // 11  011  1110  1011  101
    PMEVCNTR30_EL0    = 0xdf5e, // 11  011  1110  1011  110
    PMCCFILTR_EL0     = 0xdf7f, // 11  011  1110  1111  111
    PMEVTYPER0_EL0    = 0xdf60, // 11  011  1110  1100  000
    PMEVTYPER1_EL0    = 0xdf61, // 11  011  1110  1100  001
    PMEVTYPER2_EL0    = 0xdf62, // 11  011  1110  1100  010
    PMEVTYPER3_EL0    = 0xdf63, // 11  011  1110  1100  011
    PMEVTYPER4_EL0    = 0xdf64, // 11  011  1110  1100  100
    PMEVTYPER5_EL0    = 0xdf65, // 11  011  1110  1100  101
    PMEVTYPER6_EL0    = 0xdf66, // 11  011  1110  1100  110
    PMEVTYPER7_EL0    = 0xdf67, // 11  011  1110  1100  111
    PMEVTYPER8_EL0    = 0xdf68, // 11  011  1110  1101  000
    PMEVTYPER9_EL0    = 0xdf69, // 11  011  1110  1101  001
    PMEVTYPER10_EL0   = 0xdf6a, // 11  011  1110  1101  010
    PMEVTYPER11_EL0   = 0xdf6b, // 11  011  1110  1101  011
    PMEVTYPER12_EL0   = 0xdf6c, // 11  011  1110  1101  100
    PMEVTYPER13_EL0   = 0xdf6d, // 11  011  1110  1101  101
    PMEVTYPER14_EL0   = 0xdf6e, // 11  011  1110  1101  110
    PMEVTYPER15_EL0   = 0xdf6f, // 11  011  1110  1101  111
    PMEVTYPER16_EL0   = 0xdf70, // 11  011  1110  1110  000
    PMEVTYPER17_EL0   = 0xdf71, // 11  011  1110  1110  001
    PMEVTYPER18_EL0   = 0xdf72, // 11  011  1110  1110  010
    PMEVTYPER19_EL0   = 0xdf73, // 11  011  1110  1110  011
    PMEVTYPER20_EL0   = 0xdf74, // 11  011  1110  1110  100
    PMEVTYPER21_EL0   = 0xdf75, // 11  011  1110  1110  101
    PMEVTYPER22_EL0   = 0xdf76, // 11  011  1110  1110  110
    PMEVTYPER23_EL0   = 0xdf77, // 11  011  1110  1110  111
    PMEVTYPER24_EL0   = 0xdf78, // 11  011  1110  1111  000
    PMEVTYPER25_EL0   = 0xdf79, // 11  011  1110  1111  001
    PMEVTYPER26_EL0   = 0xdf7a, // 11  011  1110  1111  010
    PMEVTYPER27_EL0   = 0xdf7b, // 11  011  1110  1111  011
    PMEVTYPER28_EL0   = 0xdf7c, // 11  011  1110  1111  100
    PMEVTYPER29_EL0   = 0xdf7d, // 11  011  1110  1111  101
    PMEVTYPER30_EL0   = 0xdf7e  // 11  011  1110  1111  110
  };

  // Note that these do not inherit from NamedImmMapper. This class is
  // sufficiently different in its behaviour that I don't believe it's worth
  // burdening the common NamedImmMapper with abstractions only needed in
  // this one case.
  struct SysRegMapper {
    static const NamedImmMapper::Mapping SysRegPairs[];

    const NamedImmMapper::Mapping *InstPairs;
    size_t NumInstPairs;

    SysRegMapper() {}
    uint32_t fromString(StringRef Name, bool &Valid) const;
    std::string toString(uint32_t Bits, bool &Valid) const;
  };

  struct MSRMapper : SysRegMapper {
    static const NamedImmMapper::Mapping MSRPairs[];
    MSRMapper();
  };

  struct MRSMapper : SysRegMapper {
    static const NamedImmMapper::Mapping MRSPairs[];
    MRSMapper();
  };

  uint32_t ParseGenericRegister(StringRef Name, bool &Valid);
}

namespace A64TLBI {
  enum TLBIValues {
    Invalid = -1,          // Op0 Op1  CRn   CRm   Op2
    IPAS2E1IS    = 0x6401, // 01  100  1000  0000  001
    IPAS2LE1IS   = 0x6405, // 01  100  1000  0000  101
    VMALLE1IS    = 0x4418, // 01  000  1000  0011  000
    ALLE2IS      = 0x6418, // 01  100  1000  0011  000
    ALLE3IS      = 0x7418, // 01  110  1000  0011  000
    VAE1IS       = 0x4419, // 01  000  1000  0011  001
    VAE2IS       = 0x6419, // 01  100  1000  0011  001
    VAE3IS       = 0x7419, // 01  110  1000  0011  001
    ASIDE1IS     = 0x441a, // 01  000  1000  0011  010
    VAAE1IS      = 0x441b, // 01  000  1000  0011  011
    ALLE1IS      = 0x641c, // 01  100  1000  0011  100
    VALE1IS      = 0x441d, // 01  000  1000  0011  101
    VALE2IS      = 0x641d, // 01  100  1000  0011  101
    VALE3IS      = 0x741d, // 01  110  1000  0011  101
    VMALLS12E1IS = 0x641e, // 01  100  1000  0011  110
    VAALE1IS     = 0x441f, // 01  000  1000  0011  111
    IPAS2E1      = 0x6421, // 01  100  1000  0100  001
    IPAS2LE1     = 0x6425, // 01  100  1000  0100  101
    VMALLE1      = 0x4438, // 01  000  1000  0111  000
    ALLE2        = 0x6438, // 01  100  1000  0111  000
    ALLE3        = 0x7438, // 01  110  1000  0111  000
    VAE1         = 0x4439, // 01  000  1000  0111  001
    VAE2         = 0x6439, // 01  100  1000  0111  001
    VAE3         = 0x7439, // 01  110  1000  0111  001
    ASIDE1       = 0x443a, // 01  000  1000  0111  010
    VAAE1        = 0x443b, // 01  000  1000  0111  011
    ALLE1        = 0x643c, // 01  100  1000  0111  100
    VALE1        = 0x443d, // 01  000  1000  0111  101
    VALE2        = 0x643d, // 01  100  1000  0111  101
    VALE3        = 0x743d, // 01  110  1000  0111  101
    VMALLS12E1   = 0x643e, // 01  100  1000  0111  110
    VAALE1       = 0x443f  // 01  000  1000  0111  111
  };

  struct TLBIMapper : NamedImmMapper {
    const static Mapping TLBIPairs[];

    TLBIMapper();
  };

  static inline bool NeedsRegister(TLBIValues Val) {
    switch (Val) {
    case VMALLE1IS:
    case ALLE2IS:
    case ALLE3IS:
    case ALLE1IS:
    case VMALLS12E1IS:
    case VMALLE1:
    case ALLE2:
    case ALLE3:
    case ALLE1:
    case VMALLS12E1:
      return false;
    default:
      return true;
    }
  }
}

namespace AArch64II {

  enum TOF {
    //===--------------------------------------------------------------===//
    // AArch64 Specific MachineOperand flags.

    MO_NO_FLAG,

    // MO_GOT - Represents a relocation referring to the GOT entry of a given
    // symbol. Used in adrp.
    MO_GOT,

    // MO_GOT_LO12 - Represents a relocation referring to the low 12 bits of the
    // GOT entry of a given symbol. Used in ldr only.
    MO_GOT_LO12,

    // MO_DTPREL_* - Represents a relocation referring to the offset from a
    // module's dynamic thread pointer. Used in the local-dynamic TLS access
    // model.
    MO_DTPREL_G1,
    MO_DTPREL_G0_NC,

    // MO_GOTTPREL_* - Represents a relocation referring to a GOT entry
    // providing the offset of a variable from the thread-pointer. Used in
    // initial-exec TLS model where this offset is assigned in the static thread
    // block and thus known by the dynamic linker.
    MO_GOTTPREL,
    MO_GOTTPREL_LO12,

    // MO_TLSDESC_* - Represents a relocation referring to a GOT entry providing
    // a TLS descriptor chosen by the dynamic linker. Used for the
    // general-dynamic and local-dynamic TLS access models where very littls is
    // known at link-time.
    MO_TLSDESC,
    MO_TLSDESC_LO12,

    // MO_TPREL_* - Represents a relocation referring to the offset of a
    // variable from the thread pointer itself. Used in the local-exec TLS
    // access model.
    MO_TPREL_G1,
    MO_TPREL_G0_NC,

    // MO_LO12 - On a symbol operand, this represents a relocation containing
    // lower 12 bits of the address. Used in add/sub/ldr/str.
    MO_LO12
  };
}

class APFloat;

namespace A64Imms {
  bool isFPImm(const APFloat &Val, uint32_t &Imm8Bits);

  inline bool isFPImm(const APFloat &Val) {
    uint32_t Imm8;
    return isFPImm(Val, Imm8);
  }

  bool isLogicalImm(unsigned RegWidth, uint64_t Imm, uint32_t &Bits);
  bool isLogicalImmBits(unsigned RegWidth, uint32_t Bits, uint64_t &Imm);

  bool isMOVZImm(int RegWidth, uint64_t Value, int &UImm16, int &Shift);
  bool isMOVNImm(int RegWidth, uint64_t Value, int &UImm16, int &Shift);

  // We sometimes want to know whether the immediate is representable with a
  // MOVN but *not* with a MOVZ (because that would take priority).
  bool isOnlyMOVNImm(int RegWidth, uint64_t Value, int &UImm16, int &Shift);

}

} // end namespace llvm;

#endif
