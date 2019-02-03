//===- XtensaConstantPoolValue.cpp - Xtensa constantpool value ------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the Xtensa specific constantpool value class.
//
//===----------------------------------------------------------------------===//

#include "XtensaConstantPoolValue.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/Type.h"
#include "llvm/Support/raw_ostream.h"
#include <cstdlib>
using namespace llvm;

XtensaConstantPoolValue::XtensaConstantPoolValue(
    Type *Ty, unsigned id, XtensaCP::XtensaCPKind kind, bool addCurrentAddress,
    XtensaCP::XtensaCPModifier modifier)
    : MachineConstantPoolValue(Ty), LabelId(id), Kind(kind),
      Modifier(modifier), AddCurrentAddress(addCurrentAddress) {}

XtensaConstantPoolValue::XtensaConstantPoolValue(
    LLVMContext &C, unsigned id, XtensaCP::XtensaCPKind kind,
    bool addCurrentAddress, XtensaCP::XtensaCPModifier modifier)
    : MachineConstantPoolValue((Type *)Type::getInt32Ty(C)), LabelId(id),
      Kind(kind), Modifier(modifier), AddCurrentAddress(addCurrentAddress) {}

XtensaConstantPoolValue::~XtensaConstantPoolValue() {}

StringRef XtensaConstantPoolValue::getModifierText() const {
  switch (Modifier) {
  case XtensaCP::no_modifier:
    return "";
  case XtensaCP::TPOFF:
    return "@TPOFF";
  }
  llvm_unreachable("Unknown modifier!");
}

int XtensaConstantPoolValue::getExistingMachineCPValue(MachineConstantPool *CP,
                                                       unsigned Alignment) {
  llvm_unreachable("Shouldn't be calling this directly!");
}

void XtensaConstantPoolValue::addSelectionDAGCSEId(FoldingSetNodeID &ID) {
  ID.AddInteger(LabelId);
}

bool XtensaConstantPoolValue::hasSameValue(XtensaConstantPoolValue *ACPV) {
  if (ACPV->Kind == Kind) {
    if (ACPV->LabelId == LabelId)
      return true;
    // Two PC relative constpool entries containing the same GV address or
    // external symbols. FIXME: What about blockaddress?
    if (Kind == XtensaCP::CPValue || Kind == XtensaCP::CPExtSymbol)
      return true;
  }
  return false;
}

void XtensaConstantPoolValue::dump() const { errs() << "  " << *this; }

void XtensaConstantPoolValue::print(raw_ostream &O) const {}

//===----------------------------------------------------------------------===//
// XtensaConstantPoolConstant
//===----------------------------------------------------------------------===//

XtensaConstantPoolConstant::XtensaConstantPoolConstant(
    Type *Ty, const Constant *C, unsigned ID, XtensaCP::XtensaCPKind Kind,
    bool AddCurrentAddress)
    : XtensaConstantPoolValue((Type *)C->getType(), ID, Kind,
                              AddCurrentAddress),
      CVal(C) {}

XtensaConstantPoolConstant::XtensaConstantPoolConstant(
    const Constant *C, unsigned ID, XtensaCP::XtensaCPKind Kind,
    bool AddCurrentAddress)
    : XtensaConstantPoolValue((Type *)C->getType(), ID, Kind,
                              AddCurrentAddress),
      CVal(C) {}

#if 0
XtensaConstantPoolConstant *
XtensaConstantPoolConstant::Create(const Constant *C, unsigned ID) 
{
  return new XtensaConstantPoolConstant(C, ID, XtensaCP::CPValue, 0, false);
}

XtensaConstantPoolConstant *
XtensaConstantPoolConstant::Create(const GlobalValue *GV) 
{
  return new XtensaConstantPoolConstant((Type*)Type::getInt32Ty(GV->getContext()),
                                     GV, 0, XtensaCP::CPValue, 0,  false);
}
#endif
XtensaConstantPoolConstant *
XtensaConstantPoolConstant::Create(const Constant *C, unsigned ID,
                                   XtensaCP::XtensaCPKind Kind) {
  return new XtensaConstantPoolConstant(C, ID, Kind, false);
}
#if 1
XtensaConstantPoolConstant *
XtensaConstantPoolConstant::Create(const Constant *C, unsigned ID,
                                   XtensaCP::XtensaCPKind Kind,
                                   bool AddCurrentAddress) {
  return new XtensaConstantPoolConstant(C, ID, Kind, AddCurrentAddress);
}
#endif
const GlobalValue *XtensaConstantPoolConstant::getGV() const {
  return dyn_cast_or_null<GlobalValue>(CVal);
}

const BlockAddress *XtensaConstantPoolConstant::getBlockAddress() const {
  return dyn_cast_or_null<BlockAddress>(CVal);
}
#if 1
int XtensaConstantPoolConstant::getExistingMachineCPValue(
    MachineConstantPool *CP, unsigned Alignment) {
  return getExistingMachineCPValueImpl<XtensaConstantPoolConstant>(CP,
                                                                   Alignment);
}
#endif
bool XtensaConstantPoolConstant::hasSameValue(XtensaConstantPoolValue *ACPV) {
  const XtensaConstantPoolConstant *ACPC =
      dyn_cast<XtensaConstantPoolConstant>(ACPV);
  return ACPC && ACPC->CVal == CVal &&
         XtensaConstantPoolValue::hasSameValue(ACPV);
}

void XtensaConstantPoolConstant::addSelectionDAGCSEId(FoldingSetNodeID &ID) {
  ID.AddPointer(CVal);
  XtensaConstantPoolValue::addSelectionDAGCSEId(ID);
}

void XtensaConstantPoolConstant::print(raw_ostream &O) const {
  O << CVal->getName();
  XtensaConstantPoolValue::print(O);
}

XtensaConstantPoolSymbol::XtensaConstantPoolSymbol(
    LLVMContext &C, const char *s, unsigned id, bool AddCurrentAddress,
    bool PrivLinkage, XtensaCP::XtensaCPModifier Modifier)
    : XtensaConstantPoolValue(C, id, XtensaCP::CPExtSymbol, AddCurrentAddress,
                              Modifier),
      S(s), PrivateLinkage(PrivLinkage) {}

XtensaConstantPoolSymbol *
XtensaConstantPoolSymbol::Create(LLVMContext &C, const char *s, unsigned ID,
                                 bool PrivLinkage,
                                 XtensaCP::XtensaCPModifier Modifier)

{
  return new XtensaConstantPoolSymbol(C, s, ID, false, PrivLinkage, Modifier);
}

int XtensaConstantPoolSymbol::getExistingMachineCPValue(MachineConstantPool *CP,
                                                        unsigned Alignment) {
  return getExistingMachineCPValueImpl<XtensaConstantPoolSymbol>(CP, Alignment);
}

bool XtensaConstantPoolSymbol::hasSameValue(XtensaConstantPoolValue *ACPV) {
  const XtensaConstantPoolSymbol *ACPS =
      dyn_cast<XtensaConstantPoolSymbol>(ACPV);
  return ACPS && ACPS->S == S && XtensaConstantPoolValue::hasSameValue(ACPV);
}

void XtensaConstantPoolSymbol::addSelectionDAGCSEId(FoldingSetNodeID &ID) {
  ID.AddString(S);
  XtensaConstantPoolValue::addSelectionDAGCSEId(ID);
}

void XtensaConstantPoolSymbol::print(raw_ostream &O) const {
  O << S;
  XtensaConstantPoolValue::print(O);
}
#if 0
XtensaConstantPoolMBB::XtensaConstantPoolMBB(LLVMContext &C,
                                       const MachineBasicBlock *mbb,
                                       unsigned id, bool AddCurrentAddress)
  : XtensaConstantPoolValue(C, id, XtensaCP::CPMachineBasicBlock, 
        AddCurrentAddress),
    MBB(mbb) 
{
}

XtensaConstantPoolMBB *XtensaConstantPoolMBB::Create(LLVMContext &C,
                                               const MachineBasicBlock *mbb,
                                               unsigned ID,
                                               unsigned char PCAdj) 
{
  return new XtensaConstantPoolMBB(C, mbb, ID, false);
}
#endif
int XtensaConstantPoolMBB::getExistingMachineCPValue(MachineConstantPool *CP,
                                                     unsigned Alignment) {
  return getExistingMachineCPValueImpl<XtensaConstantPoolMBB>(CP, Alignment);
}

bool XtensaConstantPoolMBB::hasSameValue(XtensaConstantPoolValue *ACPV) {
  const XtensaConstantPoolMBB *ACPMBB = dyn_cast<XtensaConstantPoolMBB>(ACPV);
  return ACPMBB && ACPMBB->MBB == MBB &&
         XtensaConstantPoolValue::hasSameValue(ACPV);
}

void XtensaConstantPoolMBB::addSelectionDAGCSEId(FoldingSetNodeID &ID) {
  ID.AddPointer(MBB);
  XtensaConstantPoolValue::addSelectionDAGCSEId(ID);
}

void XtensaConstantPoolMBB::print(raw_ostream &O) const {
  O << "BB#" << MBB->getNumber();
  XtensaConstantPoolValue::print(O);
}

XtensaConstantPoolJumpTable::XtensaConstantPoolJumpTable(LLVMContext &C,
                                                         unsigned idx)
    : XtensaConstantPoolValue(C, 0, XtensaCP::CPJumpTable, false), IDX(idx) {}

XtensaConstantPoolJumpTable *XtensaConstantPoolJumpTable::Create(LLVMContext &C,
                                                                 unsigned idx) {
  return new XtensaConstantPoolJumpTable(C, idx);
}

int XtensaConstantPoolJumpTable::getExistingMachineCPValue(
    MachineConstantPool *CP, unsigned Alignment) {
  return getExistingMachineCPValueImpl<XtensaConstantPoolJumpTable>(CP,
                                                                    Alignment);
}

bool XtensaConstantPoolJumpTable::hasSameValue(XtensaConstantPoolValue *ACPV) {
  const XtensaConstantPoolJumpTable *ACPJT =
      dyn_cast<XtensaConstantPoolJumpTable>(ACPV);
  return ACPJT && ACPJT->IDX == IDX &&
         XtensaConstantPoolValue::hasSameValue(ACPV);
}

void XtensaConstantPoolJumpTable::addSelectionDAGCSEId(FoldingSetNodeID &ID) {
  //  ID.AddPointer(IDX);
  //  XtensaConstantPoolValue::addSelectionDAGCSEId(ID);
}

void XtensaConstantPoolJumpTable::print(raw_ostream &O) const {
  O << "JT" << IDX;
  XtensaConstantPoolValue::print(O);
}
