#include "XtensaAsmPrinter.h"
#include "XtensaConstantPoolValue.h"
#include "XtensaMCInstLower.h"
#include "InstPrinter/XtensaInstPrinter.h"
#include "llvm/CodeGen/MachineModuleInfoImpls.h"
#include "llvm/CodeGen/TargetLoweringObjectFileImpl.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/Support/TargetRegistry.h"

using namespace llvm;

void XtensaAsmPrinter::EmitInstruction(const MachineInstr *MI) 
{
  XtensaMCInstLower Lower(MF->getContext(), *this);
  MCInst LoweredMI;
  Lower.lower(MI, LoweredMI);
  EmitToStreamer(*OutStreamer, LoweredMI);
}

void XtensaAsmPrinter::
EmitMachineConstantPoolValue(MachineConstantPoolValue *MCPV) 
{
  const DataLayout &DL = getDataLayout();
  int Size = DL.getTypeAllocSize(MCPV->getType());

  XtensaConstantPoolValue *ACPV = static_cast<XtensaConstantPoolValue*>(MCPV);

  MCSymbol *MCSym;
  if (ACPV->isLSDA()) 
  {
    MCSym = getCurExceptionSym();
  } 
  else if (ACPV->isBlockAddress()) 
  {
    const BlockAddress *BA =
        cast<XtensaConstantPoolConstant>(ACPV)->getBlockAddress();
    MCSym = GetBlockAddressSymbol(BA);
  } 
  else if (ACPV->isGlobalValue()) 
  {
    const GlobalValue *GV = cast<XtensaConstantPoolConstant>(ACPV)->getGV();

    // TODO some modifiers
    MCSym = getSymbol(GV);
  } 
  else if (ACPV->isMachineBasicBlock()) 
  {
    const MachineBasicBlock *MBB = cast<XtensaConstantPoolMBB>(ACPV)->getMBB();
    MCSym = MBB->getSymbol();
  } 
  else if (ACPV->isJumpTable())
  {
    unsigned idx = cast<XtensaConstantPoolJumpTable>(ACPV)->getIndex();
    MCSym = this->GetJTISymbol(idx, false);
  }
  else 
  {
    assert(ACPV->isExtSymbol() && "unrecognized constant pool value");
    const char *Sym = cast<XtensaConstantPoolSymbol>(ACPV)->getSymbol();
    MCSym = GetExternalSymbolSymbol(Sym);
  }

  // Create an MCSymbol for the reference.
  const MCExpr *Expr = MCSymbolRefExpr::create(MCSym, OutContext);
  OutStreamer->EmitValue(Expr, Size);  
}

void XtensaAsmPrinter::printOperand(const MachineInstr *MI, int OpNo, raw_ostream &O) 
{
  const MachineOperand &MO = MI->getOperand(OpNo); 
  //look at target flags to see if we should wrap this operand
  /* TODO 
  switch (MO.getTargetFlags())
  {
  }
  */
  
  switch (MO.getType()) 
  {
    case MachineOperand::MO_Register:
    case MachineOperand::MO_Immediate: 
    {
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

  if(MO.getTargetFlags()) 
  {
    O << ")";
  }
}


bool XtensaAsmPrinter::PrintAsmOperand(const MachineInstr *MI,
                                        unsigned OpNo,
                                        unsigned AsmVariant,
                                        const char *ExtraCode,
                                        raw_ostream &OS)
{
  if (ExtraCode && *ExtraCode == 'n') 
  {
    if (!MI->getOperand(OpNo).isImm())
      return true;
    OS << -int64_t(MI->getOperand(OpNo).getImm());
  } 
  else 
  {
    printOperand(MI, OpNo, OS);
  }
  return false;
}

bool XtensaAsmPrinter::PrintAsmMemoryOperand(const MachineInstr *MI,
                                              unsigned OpNo,
                                              unsigned AsmVariant,
                                              const char *ExtraCode,
                                              raw_ostream &OS) 
{
  XtensaInstPrinter::printAddress(MI->getOperand(OpNo).getReg(),
                                   MI->getOperand(OpNo + 1).getImm(),
                                   OS);
  return false;
}

void XtensaAsmPrinter::printMemOperand(const MachineInstr *MI, int opNum,
                                      raw_ostream &OS) {
    OS << '%' << XtensaInstPrinter::getRegisterName(MI->getOperand(opNum).getReg());
    OS << "(";
    OS << MI->getOperand(opNum+1).getImm();
    OS << ")";
}

void XtensaAsmPrinter::EmitEndOfAsmFile(Module &M) 
{
  const Triple &TT = TM.getTargetTriple();
  if (TT.isOSBinFormatELF()) 
  {
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

bool XtensaAsmPrinter::runOnMachineFunction(MachineFunction &MF) 
{
  Subtarget = &MF.getSubtarget<XtensaSubtarget>();
  return AsmPrinter::runOnMachineFunction(MF);
}

// Force static initialization.
extern "C" void LLVMInitializeXtensaAsmPrinter() 
{
  RegisterAsmPrinter<XtensaAsmPrinter> A(TheXtensaTarget);
}
