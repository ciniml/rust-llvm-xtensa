#ifndef LLVM_LIB_TARGET_XTENSA_XTENSACONSTANTPOOLVALUE_H
#define LLVM_LIB_TARGET_XTENSA_XTENSACONSTANTPOOLVALUE_H

#include "llvm/CodeGen/MachineConstantPool.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/ErrorHandling.h"
#include <cstddef>

namespace llvm 
{

class BlockAddress;
class Constant;
class GlobalValue;
class LLVMContext;
class MachineBasicBlock;

namespace XtensaCP 
{
  enum XtensaCPKind 
  {
    CPValue,
    CPExtSymbol,
    CPBlockAddress,
    CPLSDA,
    CPMachineBasicBlock,
    CPJumpTable
  };
}

/// XtensaConstantPoolValue - Xtensa specific constantpool value. This is used to
/// represent PC-relative displacement between the address of the load
/// instruction and the constant being loaded, i.e. (&GV-(LPIC+8)).
class XtensaConstantPoolValue : public MachineConstantPoolValue 
{
  unsigned LabelId;        // Label id of the load.
  XtensaCP::XtensaCPKind Kind;   // Kind of constant.
  bool AddCurrentAddress;

protected:
  XtensaConstantPoolValue(Type *Ty, unsigned id, XtensaCP::XtensaCPKind Kind,
                       bool AddCurrentAddress);

  XtensaConstantPoolValue(LLVMContext &C, unsigned id, XtensaCP::XtensaCPKind Kind, 
                       bool AddCurrentAddress);

  template <typename Derived>
  int getExistingMachineCPValueImpl(MachineConstantPool *CP,
                                    unsigned Alignment) 
  {
    unsigned AlignMask = Alignment - 1;
    const std::vector<MachineConstantPoolEntry> &Constants = CP->getConstants();
    for (unsigned i = 0, e = Constants.size(); i != e; ++i) 
    {
      if (Constants[i].isMachineConstantPoolEntry() &&
          (Constants[i].getAlignment() & AlignMask) == 0) 
      {
        XtensaConstantPoolValue *CPV =
            (XtensaConstantPoolValue *)Constants[i].Val.MachineCPVal;
        if (Derived *APC = dyn_cast<Derived>(CPV))
          if (cast<Derived>(this)->equals(APC))
            return i;
      }
    }

    return -1;
  }

public:
  ~XtensaConstantPoolValue() override;

  bool mustAddCurrentAddress() const { return AddCurrentAddress; }

  unsigned getLabelId() const { return LabelId; }
  void setLabelId(unsigned id) { LabelId = id; }

  bool isGlobalValue() const { return Kind == XtensaCP::CPValue; }
  bool isExtSymbol() const { return Kind == XtensaCP::CPExtSymbol; }
  bool isBlockAddress() const { return Kind == XtensaCP::CPBlockAddress; }
  bool isLSDA() const { return Kind == XtensaCP::CPLSDA; }
  bool isMachineBasicBlock() const { return Kind == XtensaCP::CPMachineBasicBlock; }
  bool isJumpTable() const { return Kind == XtensaCP::CPJumpTable; }

  int getExistingMachineCPValue(MachineConstantPool *CP,
                                unsigned Alignment) override;

  void addSelectionDAGCSEId(FoldingSetNodeID &ID) override;

  /// hasSameValue - Return true if this Xtensa constpool value can share the same
  /// constantpool entry as another Xtensa constpool value.
  virtual bool hasSameValue(XtensaConstantPoolValue *ACPV);

  bool equals(const XtensaConstantPoolValue *A) const 
  {
    return this->LabelId == A->LabelId;
  }

  void print(raw_ostream &O) const override;
  void print(raw_ostream *O) const { if (O) print(*O); }
  void dump() const;
};

inline raw_ostream &operator<<(raw_ostream &O, const XtensaConstantPoolValue &V) 
{
  V.print(O);
  return O;
}

/// XtensaConstantPoolConstant - Xtensa-specific constant pool values for Constants,
/// Functions, and BlockAddresses.
class XtensaConstantPoolConstant : public XtensaConstantPoolValue 
{
  const Constant *CVal;         // Constant being loaded.

  XtensaConstantPoolConstant(const Constant *C,
                          unsigned ID,
                          XtensaCP::XtensaCPKind Kind,
                          bool AddCurrentAddress);
  XtensaConstantPoolConstant(Type *Ty, const Constant *C,
                          unsigned ID,
                          XtensaCP::XtensaCPKind Kind,
                          bool AddCurrentAddress);

public:
#if 0
  static XtensaConstantPoolConstant *Create(const Constant *C, unsigned ID);
  static XtensaConstantPoolConstant *Create(const GlobalValue *GV);
#endif
  static XtensaConstantPoolConstant *Create(const Constant *C, unsigned ID,
                                         XtensaCP::XtensaCPKind Kind);
  static XtensaConstantPoolConstant *Create(const Constant *C, unsigned ID,
                                         XtensaCP::XtensaCPKind Kind,
                                         bool AddCurrentAddress);

  const GlobalValue *getGV() const;
  const BlockAddress *getBlockAddress() const;
#if 0
  int getExistingMachineCPValue(MachineConstantPool *CP,
                                unsigned Alignment) override;
#endif
  /// hasSameValue - Return true if this Xtensa constpool value can share the same
  /// constantpool entry as another Xtensa constpool value.
  bool hasSameValue(XtensaConstantPoolValue *ACPV) override;

  void addSelectionDAGCSEId(FoldingSetNodeID &ID) override;

  void print(raw_ostream &O) const override;
  static bool classof(const XtensaConstantPoolValue *APV) 
  {
    return APV->isGlobalValue() || APV->isBlockAddress() || APV->isLSDA();
  }

  bool equals(const XtensaConstantPoolConstant *A) const 
  {
    return CVal == A->CVal && XtensaConstantPoolValue::equals(A);
  }
};

/// XtensaConstantPoolSymbol - Xtensa-specific constantpool values for external
/// symbols.
class XtensaConstantPoolSymbol : public XtensaConstantPoolValue 
{
  const std::string S;          // ExtSymbol being loaded.

  XtensaConstantPoolSymbol(LLVMContext &C, const char *s, unsigned id,
                           bool AddCurrentAddress);

public:
  static XtensaConstantPoolSymbol *Create(LLVMContext &C, const char *s,
                                       unsigned ID);

  const char *getSymbol() const { return S.c_str(); }

  int getExistingMachineCPValue(MachineConstantPool *CP,
                                unsigned Alignment) override;

  void addSelectionDAGCSEId(FoldingSetNodeID &ID) override;

  /// hasSameValue - Return true if this Xtensa constpool value can share the same
  /// constantpool entry as another Xtensa constpool value.
  bool hasSameValue(XtensaConstantPoolValue *ACPV) override;

  void print(raw_ostream &O) const override;

  static bool classof(const XtensaConstantPoolValue *ACPV) 
  {
    return ACPV->isExtSymbol();
  }

  bool equals(const XtensaConstantPoolSymbol *A) const 
  {
    return S == A->S && XtensaConstantPoolValue::equals(A);
  }
};

/// XtensaConstantPoolMBB - Xtensa-specific constantpool value of a machine basic
/// block.
class XtensaConstantPoolMBB : public XtensaConstantPoolValue 
{
  const MachineBasicBlock *MBB; // Machine basic block.

  XtensaConstantPoolMBB(LLVMContext &C, const MachineBasicBlock *mbb, unsigned id,
                     unsigned char PCAdj, bool AddCurrentAddress);

public:
  static XtensaConstantPoolMBB *Create(LLVMContext &C,
                                    const MachineBasicBlock *mbb,
                                    unsigned ID, unsigned char PCAdj);

  const MachineBasicBlock *getMBB() const { return MBB; }

  int getExistingMachineCPValue(MachineConstantPool *CP,
                                unsigned Alignment) override;

  void addSelectionDAGCSEId(FoldingSetNodeID &ID) override;

  /// hasSameValue - Return true if this Xtensa constpool value can share the same
  /// constantpool entry as another Xtensa constpool value.
  bool hasSameValue(XtensaConstantPoolValue *ACPV) override;

  void print(raw_ostream &O) const override;

  static bool classof(const XtensaConstantPoolValue *ACPV) 
  {
    return ACPV->isMachineBasicBlock();
  }

  bool equals(const XtensaConstantPoolMBB *A) const 
  {
    return MBB == A->MBB && XtensaConstantPoolValue::equals(A);
  }
};

/// XtensaConstantPoolJumpTable - Xtensa-specific constantpool values for Jump Table
/// symbols.
class XtensaConstantPoolJumpTable : public XtensaConstantPoolValue 
{
  unsigned IDX; // Jump Table Index.

  XtensaConstantPoolJumpTable(LLVMContext &C, unsigned idx);

public:
  static XtensaConstantPoolJumpTable *Create(LLVMContext &C, unsigned idx);

  unsigned getIndex() const { return IDX; }

  int getExistingMachineCPValue(MachineConstantPool *CP,
                                unsigned Alignment) override;

  void addSelectionDAGCSEId(FoldingSetNodeID &ID) override;

  /// hasSameValue - Return true if this Xtensa constpool value can share the
  /// same constantpool entry as another Xtensa constpool value.
  bool hasSameValue(XtensaConstantPoolValue *ACPV) override;

  void print(raw_ostream &O) const override;

  static bool classof(const XtensaConstantPoolValue *ACPV) 
  {
    return ACPV->isJumpTable();
  }

  bool equals(const XtensaConstantPoolJumpTable *A) const 
  {
    return IDX== A->IDX && XtensaConstantPoolValue::equals(A);
  }
};

} // End llvm namespace

#endif /* LLVM_LIB_TARGET_XTENSA_XTENSACONSTANTPOOLVALUE_H */

