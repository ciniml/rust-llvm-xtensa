#include "InstPrinter/XtensaInstPrinter.h"
#include "XtensaAsmPrinter.h"
#include "XtensaConstantPoolValue.h"
#include "XtensaMCInstLower.h"
#include "llvm/CodeGen/MachineModuleInfoImpls.h"
#include "llvm/CodeGen/TargetLoweringObjectFileImpl.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/Support/TargetRegistry.h"

using namespace llvm;

void XtensaAsmPrinter::EmitInstruction(const MachineInstr *MI) {
  XtensaMCInstLower Lower(MF->getContext(), *this);
  MCInst LoweredMI;
  Lower.lower(MI, LoweredMI);
  EmitToStreamer(*OutStreamer, LoweredMI);
}

/// EmitConstantPool - Print to the current output stream assembly
/// representations of the constants in the constant pool MCP. This is
/// used to print out constants which have been "spilled to memory" by
/// the code generator.
void XtensaAsmPrinter::EmitConstantPool() {
  const MachineConstantPool *MCP = MF->getConstantPool();
  const std::vector<MachineConstantPoolEntry> &CP = MCP->getConstants();
  if (CP.empty())
    return;

  OutStreamer->EmitRawText("\t.text");
  OutStreamer->EmitRawText("\t.literal_position");
  for (unsigned i = 0, e = CP.size(); i != e; ++i) {
    const MachineConstantPoolEntry &CPE = CP[i];

    if (CPE.isMachineConstantPoolEntry()) {
      XtensaConstantPoolValue *ACPV =
          static_cast<XtensaConstantPoolValue *>(CPE.Val.MachineCPVal);
      ACPV->setLabelId(i);
      EmitMachineConstantPoolValue(CPE.Val.MachineCPVal);
    } else {
      MCSymbol *LblSym = GetCPISymbol(i);

      std::string str("\t.literal ");
      str += LblSym->getName();
      str += ", ";
      if (CPE.Val.ConstVal->getType()->getTypeID() == llvm::Type::FloatTyID) {
        const ConstantFP *CFPVal =
            static_cast<const ConstantFP *>(CPE.Val.ConstVal);
        str += CFPVal->getValueAPF().bitcastToAPInt().toString(10, true);
      } else {
        const ConstantInt *CVal =
            static_cast<const ConstantInt *>(CPE.Val.ConstVal);
        str += CVal->getValue().toString(10, true);
      }


      OutStreamer->EmitRawText(str);   
    }
  }
}

void XtensaAsmPrinter::EmitMachineConstantPoolValue(
    MachineConstantPoolValue *MCPV) {
  const DataLayout &DL = getDataLayout();
  int Size = DL.getTypeAllocSize(MCPV->getType());

  XtensaConstantPoolValue *ACPV = static_cast<XtensaConstantPoolValue *>(MCPV);

  MCSymbol *MCSym;
  if (ACPV->isLSDA()) {
    MCSym = getCurExceptionSym();
  } else if (ACPV->isBlockAddress()) {
    const BlockAddress *BA =
        cast<XtensaConstantPoolConstant>(ACPV)->getBlockAddress();
    MCSym = GetBlockAddressSymbol(BA);
  } else if (ACPV->isGlobalValue()) {
    const GlobalValue *GV = cast<XtensaConstantPoolConstant>(ACPV)->getGV();

    // TODO some modifiers
    MCSym = getSymbol(GV);
  } else if (ACPV->isMachineBasicBlock()) {
    const MachineBasicBlock *MBB = cast<XtensaConstantPoolMBB>(ACPV)->getMBB();
    MCSym = MBB->getSymbol();
  } else if (ACPV->isJumpTable()) {
    unsigned idx = cast<XtensaConstantPoolJumpTable>(ACPV)->getIndex();
    MCSym = this->GetJTISymbol(idx, false);
  } else {
    assert(ACPV->isExtSymbol() && "unrecognized constant pool value");
    XtensaConstantPoolSymbol *XtensaSym = cast<XtensaConstantPoolSymbol>(ACPV);
    const char *Sym = XtensaSym->getSymbol();
	// TODO it's a trick to distinguish static references and generated rodata references
	// Some clear method required
    if (strchr(Sym, '.'))
    {
      char buf[256];
      if (XtensaSym->isPrivateLinkage())
        sprintf(buf, ".L%s", Sym);
      else
        sprintf(buf, "%s", Sym);
      MCSym = GetExternalSymbolSymbol(buf);
    } else
      MCSym = GetExternalSymbolSymbol(Sym);
  }

  // Create an MCSymbol for the reference.
  //  const MCExpr *Expr = MCSymbolRefExpr::create(MCSym, OutContext);
  //  OutStreamer->EmitValue(Expr, Size);

  MCSymbol *LblSym = GetCPISymbol(ACPV->getLabelId());

  std::string str("\t.literal ");
  str += LblSym->getName();
  str += ", ";
  str += MCSym->getName();

  OutStreamer->EmitRawText(str);
}

void XtensaAsmPrinter::printOperand(const MachineInstr *MI, int OpNo,
                                    raw_ostream &O) {
  const MachineOperand &MO = MI->getOperand(OpNo);
  // look at target flags to see if we should wrap this operand
  /* TODO
  switch (MO.getTargetFlags())
  {
  }
  */

  switch (MO.getType()) {
  case MachineOperand::MO_Register:
  case MachineOperand::MO_Immediate: {
    XtensaMCInstLower Lower(MF->getContext(), *this);
    MCOperand MC(Lower.lowerOperand(MI->getOperand(OpNo)));
    XtensaInstPrinter::printOperand(MC, O);
    break;
  }
  case MachineOperand::MO_GlobalAddress:
    O << *getSymbol(MO.getGlobal());
    break;
  default:
    llvm_unreachable("<unknown operand type>");
  }

  if (MO.getTargetFlags()) {
    O << ")";
  }
}

bool XtensaAsmPrinter::PrintAsmOperand(const MachineInstr *MI, unsigned OpNo,
                                       unsigned AsmVariant,
                                       const char *ExtraCode, raw_ostream &OS) {
  if (ExtraCode && *ExtraCode == 'n') {
    if (!MI->getOperand(OpNo).isImm())
      return true;
    OS << -int64_t(MI->getOperand(OpNo).getImm());
  } else {
    printOperand(MI, OpNo, OS);
  }
  return false;
}

bool XtensaAsmPrinter::PrintAsmMemoryOperand(const MachineInstr *MI,
                                             unsigned OpNo, unsigned AsmVariant,
                                             const char *ExtraCode,
                                             raw_ostream &OS) {
  XtensaInstPrinter::printAddress(MI->getOperand(OpNo).getReg(),
                                  MI->getOperand(OpNo + 1).getImm(), OS);
  return false;
}

void XtensaAsmPrinter::printMemOperand(const MachineInstr *MI, int opNum,
                                       raw_ostream &OS) {
  OS << '%'
     << XtensaInstPrinter::getRegisterName(MI->getOperand(opNum).getReg());
  OS << "(";
  OS << MI->getOperand(opNum + 1).getImm();
  OS << ")";
}

void XtensaAsmPrinter::EmitEndOfAsmFile(Module &M) {
  const Triple &TT = TM.getTargetTriple();
  if (TT.isOSBinFormatELF()) {
    const TargetLoweringObjectFileELF &TLOFELF =
        static_cast<const TargetLoweringObjectFileELF &>(getObjFileLowering());

    MachineModuleInfoELF &MMIELF = MMI->getObjFileInfo<MachineModuleInfoELF>();

    // Output stubs for external and common global variables.
    MachineModuleInfoELF::SymbolListTy Stubs = MMIELF.GetGVStubList();
    if (!Stubs.empty()) {
      OutStreamer->SwitchSection(TLOFELF.getDataSection());
      const DataLayout TD = getDataLayout();

      for (unsigned i = 0, e = Stubs.size(); i != e; ++i) {
        OutStreamer->EmitLabel(Stubs[i].first);
        OutStreamer->EmitSymbolValue(Stubs[i].second.getPointer(),
                                     TD.getPointerSize(0), 0);
      }
      Stubs.clear();
    }
  }
}

bool XtensaAsmPrinter::runOnMachineFunction(MachineFunction &MF) {
  Subtarget = &MF.getSubtarget<XtensaSubtarget>();
  return AsmPrinter::runOnMachineFunction(MF);
}

// Force static initialization.
extern "C" void LLVMInitializeXtensaAsmPrinter() {
  RegisterAsmPrinter<XtensaAsmPrinter> A(TheXtensaTarget);
}
