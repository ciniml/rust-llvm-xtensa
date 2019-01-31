//===-- XtensaMCAsmBackend.cpp - Xtensa assembler backend ---------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===--------------------------------------------------------------------===//

#include "MCTargetDesc/XtensaMCFixupKinds.h"
#include "MCTargetDesc/XtensaMCTargetDesc.h"
#include "llvm/MC/MCAsmBackend.h"
#include "llvm/MC/MCELFObjectWriter.h"
#include "llvm/MC/MCFixupKindInfo.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/MC/MCSubtargetInfo.h"

using namespace llvm;

namespace {
class XtensaMCAsmBackend : public MCAsmBackend {
  uint8_t OSABI;

public:
  XtensaMCAsmBackend(uint8_t osABI) : OSABI(osABI) {}

  // Override MCAsmBackend
  unsigned getNumFixupKinds() const override {
    return Xtensa::NumTargetFixupKinds;
  }
  const MCFixupKindInfo &getFixupKindInfo(MCFixupKind Kind) const override;
  void applyFixup(const MCAssembler &Asm, const MCFixup &Fixup,
                  const MCValue &Target, MutableArrayRef<char> Data,
                  uint64_t Value, bool IsResolved) const override;
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
XtensaMCAsmBackend::getFixupKindInfo(MCFixupKind Kind) const {
  assert((Kind < FirstTargetFixupKind) && "Invalid kind!");
  return MCAsmBackend::getFixupKindInfo(Kind);
}

void XtensaMCAsmBackend::applyFixup(const MCAssembler &Asm,
                                    const MCFixup &Fixup, const MCValue &Target,
                                    MutableArrayRef<char> Data, uint64_t Value,
                                    bool IsResolved) const {}

bool XtensaMCAsmBackend::mayNeedRelaxation(const MCInst &Inst) const {
  return false;
}

bool XtensaMCAsmBackend::fixupNeedsRelaxation(
    const MCFixup &Fixup, uint64_t Value, const MCRelaxableFragment *Fragment,
    const MCAsmLayout &Layout) const {
  return false;
}

void XtensaMCAsmBackend::relaxInstruction(const MCInst &Inst,
                                          const MCSubtargetInfo &STI,
                                          MCInst &Res) const {}

bool XtensaMCAsmBackend::writeNopData(uint64_t Count,
                                      MCObjectWriter *OW) const {
  for (uint64_t I = 0; I != Count; ++I)
    OW->write8(7);
  return true;
}

MCAsmBackend *llvm::createXtensaMCAsmBackend(const Target &T,
                                             const MCSubtargetInfo &STI,
                                             const MCRegisterInfo &MRI,
                                             const MCTargetOptions &Options) {
  uint8_t OSABI =
      MCELFObjectTargetWriter::getOSABI(STI.getTargetTriple().getOS());
  return new XtensaMCAsmBackend(OSABI);
}
