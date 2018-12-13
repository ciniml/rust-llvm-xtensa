#include "MCTargetDesc/XtensaMCTargetDesc.h"
#include "MCTargetDesc/XtensaMCFixups.h"
#include "llvm/MC/MCAsmBackend.h"
#include "llvm/MC/MCELFObjectWriter.h"
#include "llvm/MC/MCFixupKindInfo.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/MC/MCSubtargetInfo.h"

using namespace llvm;

// Value is a fully-resolved relocation value: Symbol + Addend [- Pivot].
// Return the bits that should be installed in a relocation field for
// fixup kind Kind.
static uint64_t extractBitsForFixup(MCFixupKind Kind, uint64_t Value) 
{
  if (Kind < FirstTargetFixupKind)
    return Value;

  switch (unsigned(Kind)) 
  {
    case Xtensa::fixup_xtensa_brlo:
    case Xtensa::fixup_xtensa_brhi:
    case Xtensa::fixup_xtensa_jal:
    return (int64_t)Value / 2;
  }

  llvm_unreachable("Unknown fixup kind!");
}

// If Opcode can be relaxed, return the relaxed form, otherwise return 0.
static unsigned getRelaxedOpcode(unsigned Opcode) 
{
  switch (Opcode) 
  {
  //case Xtensa::BRC:  return Xtensa::BRCL;
  //case Xtensa::J:    return Xtensa::JG;
  //case Xtensa::BRAS: return Xtensa::BRASL;
  }
  return 0;
}

namespace 
{
class XtensaMCAsmBackend : public MCAsmBackend 
{
  uint8_t OSABI;
public:
  XtensaMCAsmBackend(uint8_t osABI)
    : OSABI(osABI) {}

  // Override MCAsmBackend
  unsigned getNumFixupKinds() const override 
  {
    return Xtensa::NumTargetFixupKinds;
  }
  const MCFixupKindInfo &getFixupKindInfo(MCFixupKind Kind) const override;
  void applyFixup(const MCAssembler &Asm, const MCFixup &Fixup,
                                const MCValue &Target,
                                MutableArrayRef<char> Data, uint64_t Value,
                                bool IsResolved) const override;
  bool mayNeedRelaxation(const MCInst &Inst) const override;
  bool fixupNeedsRelaxation(const MCFixup &Fixup, uint64_t Value,
                            const MCRelaxableFragment *Fragment,
                            const MCAsmLayout &Layout) const override;
  void relaxInstruction(const MCInst &Inst, const MCSubtargetInfo &STI,
                        MCInst &Res) const override;
  bool writeNopData(uint64_t Count, MCObjectWriter *OW) const override;

  std::unique_ptr<MCObjectWriter>
  createObjectWriter(raw_pwrite_stream &OS) const override {
    return createXtensaObjectWriter(OS, OSABI);
  }

};
} // end anonymous namespace

const MCFixupKindInfo &
XtensaMCAsmBackend::getFixupKindInfo(MCFixupKind Kind) const
{
  const static MCFixupKindInfo Infos[Xtensa::NumTargetFixupKinds] = 
  {
    { "fixup_xtensa_brlo",  10, 7, MCFixupKindInfo::FKF_IsPCRel },
    { "fixup_xtensa_brhi",  27, 5, MCFixupKindInfo::FKF_IsPCRel },
    { "fixup_xtensa_jal", 0, 20, MCFixupKindInfo::FKF_IsPCRel },
    // target offset(0) doesn't make sense here: bits are non continuous
  };

  if (Kind < FirstTargetFixupKind)
    return MCAsmBackend::getFixupKindInfo(Kind);

  assert(unsigned(Kind - FirstTargetFixupKind) < getNumFixupKinds() &&
         "Invalid kind!");
  return Infos[Kind - FirstTargetFixupKind];
}

void XtensaMCAsmBackend::applyFixup(const MCAssembler &Asm, const MCFixup &Fixup,
                                const MCValue &Target,
                                MutableArrayRef<char> Data, uint64_t Value,
                                bool IsResolved) const {
  MCFixupKind Kind = Fixup.getKind();
  unsigned Offset = Fixup.getOffset();
  unsigned BitSize = getFixupKindInfo(Kind).TargetSize;
  unsigned Size = (BitSize + 7) / 8;

  assert(Offset + Size <= Data.size() && "Invalid fixup offset!");

  // Big-endian insertion of Size bytes.
  Value = extractBitsForFixup(Kind, Value);
  if (BitSize < 64)
    Value &= ((uint64_t)1 << BitSize) - 1;
  unsigned ShiftValue = (Size * 8) - 8;
  for (unsigned I = 0; I != Size; ++I) {
    Data[Offset + I] |= uint8_t(Value >> ShiftValue);
    ShiftValue -= 8;
  }
}


bool XtensaMCAsmBackend::mayNeedRelaxation(const MCInst &Inst) const 
{
  return getRelaxedOpcode(Inst.getOpcode()) != 0;
}

bool
XtensaMCAsmBackend::fixupNeedsRelaxation(const MCFixup &Fixup,
                                          uint64_t Value,
                                          const MCRelaxableFragment *Fragment,
                                          const MCAsmLayout &Layout) const 
{
  // At the moment we just need to relax 16-bit fields to wider fields.
  Value = extractBitsForFixup(Fixup.getKind(), Value);
  return (int16_t)Value != (int64_t)Value;
}

void XtensaMCAsmBackend::relaxInstruction(const MCInst &Inst, const MCSubtargetInfo &STI,
                        MCInst &Res) const 
{
  unsigned Opcode = getRelaxedOpcode(Inst.getOpcode());
  assert(Opcode && "Unexpected insn to relax");
  Res = Inst;
  Res.setOpcode(Opcode);
}

bool XtensaMCAsmBackend::writeNopData(uint64_t Count,
                                       MCObjectWriter *OW) const 
{
  for (uint64_t I = 0; I != Count; ++I)
    OW->write8(7);
  return true;
}

#if 0
MCAsmBackend *llvm::createXtensaMCAsmBackend(const Target &T,
                                            const MCRegisterInfo &MRI,
                                            const Triple &TT, StringRef CPU) 
{
  uint8_t OSABI = MCELFObjectTargetWriter::getOSABI(TT.getOS());
  return new XtensaMCAsmBackend(OSABI);
}
#else
MCAsmBackend *llvm::createXtensaMCAsmBackend(const Target &T,
                                              const MCSubtargetInfo &STI,
                                              const MCRegisterInfo &MRI,
                                              const MCTargetOptions &Options) {
  uint8_t OSABI =
      MCELFObjectTargetWriter::getOSABI(STI.getTargetTriple().getOS());
  return new XtensaMCAsmBackend(OSABI);
}
#endif
