#include "XtensaTargetMachine.h"
#include "llvm/CodeGen/SelectionDAGISel.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

#define DEBUG_TYPE "xtensa-isel"

namespace 
{
// Used to build addressing modes.
struct XtensaAddressingMode 
{
  // The shape of the address.
  enum AddrForm 
  {
    // base+offset
    FormBO
  };
  AddrForm Form;

  // The type of displacement. 
  enum OffRange 
  {
    Off12Only
  };
  OffRange OffR;

  // The parts of the address.  The address is equivalent to:
  //
  //     Base + Offset + Index + (IncludesDynAlloc ? ADJDYNALLOC : 0)
  SDValue Base;
  int64_t Offset;

  XtensaAddressingMode(AddrForm form, OffRange offr)
    : Form(form), OffR(offr), Base(), Offset(0) {}

  void dump() 
  {
    errs() << "XtensaAddressingMode " << this << '\n';

    errs() << " Base ";
    if (Base.getNode() != 0)
      Base.getNode()->dump();
    else
      errs() << "null\n";

    errs() << " Offset " << Offset;
  }
};

class XtensaDAGToDAGISel : public SelectionDAGISel 
{
  const XtensaTargetLowering &Lowering;
  const XtensaSubtarget &Subtarget;

  // Used by XtensaOperands.td to create integer constants.
  inline SDValue getImm(const SDNode *Node, uint64_t Imm) 
  {
    return CurDAG->getTargetConstant(Imm, SDLoc(Node), Node->getValueType(0));
  }
  /// getI32Imm - Return a target constant with the specified value, of type
  /// i32.
  SDValue getI32Imm(unsigned Imm, SDLoc DL) 
  {
    return CurDAG->getTargetConstant(Imm, DL, MVT::i32);
  }

  // Try to fold more of the base or index of AM into AM, where IsBase
  // selects between the base and index.
  bool expandAddress(XtensaAddressingMode &AM, bool IsBase);

  // Try to describe N in AM, returning true on success.
  bool selectAddress(SDValue N, XtensaAddressingMode &AM);

  // Extract individual target operands from matched address AM.
  void getAddressOperands(const XtensaAddressingMode &AM, EVT VT,
                          SDValue &Base, SDValue &Disp);
  void getAddressOperands(const XtensaAddressingMode &AM, EVT VT,
                          SDValue &Base, SDValue &Disp, SDValue &Index);

  bool selectMemRegAddr(SDValue Addr, SDValue &Offset, SDValue &Base) 
  {
    EVT ValTy = Addr.getValueType();

    // if Address is FI, get the TargetFrameIndex.
    if (FrameIndexSDNode *FIN = dyn_cast<FrameIndexSDNode>(Addr)) 
    {
      Base   = CurDAG->getTargetFrameIndex(FIN->getIndex(), ValTy);
      Offset = CurDAG->getTargetConstant(0, SDLoc(Addr), ValTy);
      return true;
    }

    if (TM.getRelocationModel() != Reloc::PIC_) 
    {
      if ((Addr.getOpcode() == ISD::TargetExternalSymbol ||
          Addr.getOpcode() == ISD::TargetGlobalAddress))
        return false;
    }

    // Addresses of the form FI+const or FI|const
    if (CurDAG->isBaseWithConstantOffset(Addr)) 
    {
      ConstantSDNode *CN = dyn_cast<ConstantSDNode>(Addr.getOperand(1));
      if (isInt<12>(CN->getSExtValue())) {
  
        // If the first operand is a FI, get the TargetFI Node
        if (FrameIndexSDNode *FIN = dyn_cast<FrameIndexSDNode>
                                    (Addr.getOperand(0)))
          Base = CurDAG->getTargetFrameIndex(FIN->getIndex(), ValTy);
        else
          Base = Addr.getOperand(0);
  
        Offset = CurDAG->getTargetConstant(CN->getZExtValue(), SDLoc(Addr), ValTy);
        return true;
      }
    }

    //Last case
    Base = Addr;
    Offset = CurDAG->getTargetConstant(0, SDLoc(Addr), Addr.getValueType());
    return true;
  }

  bool selectRegAddr(SDValue Addr, SDValue &Base) 
  {
    //always just register
    Base = Addr;
    return true;
  }

  // PC-relative address matching routines used by XtensaOperands.td.
  bool selectPCRelAddress(SDValue Addr, SDValue &Target) 
  {
    if (Addr.getOpcode() == XtensaISD::PCREL_WRAPPER) 
    {
      Target = Addr.getOperand(0);
      return true;
    }
    return false;
  }

  // If Op0 is null, then Node is a constant that can be loaded using:
  //
  //   (Opcode UpperVal LowerVal)
  //
  // If Op0 is nonnull, then Node can be implemented using:
  //
  //   (Opcode (Opcode Op0 UpperVal) LowerVal)
  SDNode *splitLargeImmediate(unsigned Opcode, SDNode *Node, SDValue Op0,
                              uint64_t UpperVal, uint64_t LowerVal);

public:
  XtensaDAGToDAGISel(XtensaTargetMachine &TM, CodeGenOpt::Level OptLevel)
    : SelectionDAGISel(TM, OptLevel),
      Lowering(*TM.getSubtargetImpl()->getTargetLowering()),
      Subtarget(*TM.getSubtargetImpl()) { }

  // Override MachineFunctionPass.
  StringRef getPassName() const override {
    return "Xtensa DAG->DAG Pattern Instruction Selection";
  }

  // Override SelectionDAGISel.
  virtual bool runOnMachineFunction(MachineFunction &MF);
  void Select(SDNode *Node) override;
  virtual void processFunctionAfterISel(MachineFunction &MF);
  bool SelectInlineAsmMemoryOperand(const SDValue &Op, unsigned ConstraintID,
                                    std::vector<SDValue> &OutOps) override;

  // Include the pieces autogenerated from the target description.
  #include "XtensaGenDAGISel.inc"
};
} // end anonymous namespace

bool XtensaDAGToDAGISel::runOnMachineFunction(MachineFunction &MF) 
{
  bool ret = SelectionDAGISel::runOnMachineFunction(MF);

  processFunctionAfterISel(MF);

  return ret;
}

FunctionPass *llvm::createXtensaISelDag(XtensaTargetMachine &TM,
                                         CodeGenOpt::Level OptLevel) 
{
  return new XtensaDAGToDAGISel(TM, OptLevel);
}

// Return true if Val should be selected as a displacement for an address
// with range DR.  Here we're interested in the range of both the instruction
// described by DR and of any pairing instruction.
static bool selectOffset(XtensaAddressingMode::OffRange OffR, int64_t Val) 
{
  switch (OffR) 
  {
  case XtensaAddressingMode::Off12Only:
    return isInt<12>(Val);
  }
  llvm_unreachable("Unhandled offset range");
}

// The base or index of AM is equivalent to Op0 + Op1, where IsBase selects
// between the base and index.  Try to fold Op1 into AM's displacement.
static bool expandOffset(XtensaAddressingMode &AM, bool IsBase,
                       SDValue Op0, ConstantSDNode *Op1) 
{
  // First try adjusting the displacement.
  int64_t TestOffset = AM.Offset + Op1->getSExtValue();
  if (selectOffset(AM.OffR, TestOffset)) 
  {
    //changeComponent(AM, IsBase, Op0);
    AM.Base = Op0;
    AM.Offset = TestOffset;
    return true;
  }

  // We could consider forcing the displacement into a register and
  // using it as an index, but it would need to be carefully tuned.
  return false;
}

bool XtensaDAGToDAGISel::expandAddress(XtensaAddressingMode &AM,
                                        bool IsBase) 
{
  //SDValue N = IsBase ? AM.Base : AM.Index;
  SDValue N = AM.Base;
  unsigned Opcode = N.getOpcode();
  if (Opcode == ISD::TRUNCATE) {
    N = N.getOperand(0);
    Opcode = N.getOpcode();
  }
  if (Opcode == ISD::ADD || CurDAG->isBaseWithConstantOffset(N)) 
  {
    SDValue Op0 = N.getOperand(0);
    SDValue Op1 = N.getOperand(1);

    unsigned Op0Code = Op0->getOpcode();
    unsigned Op1Code = Op1->getOpcode();

    if (Op0Code == ISD::Constant)
      return expandOffset(AM, IsBase, Op1, cast<ConstantSDNode>(Op0));
    if (Op1Code == ISD::Constant)
      return expandOffset(AM, IsBase, Op0, cast<ConstantSDNode>(Op1));

  }
  return false;
}

// Return true if an instruction with displacement range DR should be
// used for displacement value Val.  selectDisp(DR, Val) must already hold.
static bool isValidOffset(XtensaAddressingMode::OffRange OffR, int64_t Val) 
{
  assert(selectOffset(OffR, Val) && "Invalid displacement");
  switch (OffR) 
  {
  case XtensaAddressingMode::Off12Only:
    return true;
  }
  llvm_unreachable("Unhandled displacement range");
}

// Return true if Addr is suitable for AM, updating AM if so.
bool XtensaDAGToDAGISel::selectAddress(SDValue Addr,
                                        XtensaAddressingMode &AM) 
{
  // Start out assuming that the address will need to be loaded separately,
  // then try to extend it as much as we can.
  AM.Base = Addr;

  // First try treating the address as a constant.
  if (Addr.getOpcode() == ISD::Constant &&
      expandOffset(AM, true, SDValue(), cast<ConstantSDNode>(Addr)))
  { }

  // Reject cases where the other instruction in a pair should be used.
  if (!isValidOffset(AM.OffR, AM.Offset))
    return false;

  DEBUG(AM.dump());
  return true;
}

// Insert a node into the DAG at least before Pos.  This will reposition
// the node as needed, and will assign it a node ID that is <= Pos's ID.
// Note that this does *not* preserve the uniqueness of node IDs!
// The selection DAG must no longer depend on their uniqueness when this
// function is used.
static void insertDAGNode(SelectionDAG *DAG, SDNode *Pos, SDValue N) 
{
  if (N.getNode()->getNodeId() == -1 ||
      N.getNode()->getNodeId() > Pos->getNodeId()) 
  {
    DAG->RepositionNode(Pos->getIterator(), N.getNode());
    N.getNode()->setNodeId(Pos->getNodeId());
  }
}

void XtensaDAGToDAGISel::getAddressOperands(const XtensaAddressingMode &AM,
                                             EVT VT, SDValue &Base,
                                             SDValue &Offset) 
{
  Base = AM.Base;
  if (!Base.getNode())
    // Register 0 means "no base".  This is mostly useful for shifts.
    Base = CurDAG->getRegister(0, VT);
  else if (Base.getOpcode() == ISD::FrameIndex) 
  {
    // Lower a FrameIndex to a TargetFrameIndex.
    int64_t FrameIndex = cast<FrameIndexSDNode>(Base)->getIndex();
    Offset = CurDAG->getTargetFrameIndex(FrameIndex, VT);
    Base = CurDAG->getTargetConstant(AM.Offset, SDLoc(Base), VT);
    return;
  } 
  else if (Base.getValueType() != VT) 
  {
    // Truncate values from i64 to i32, for shifts.
    assert(VT == MVT::i32 && Base.getValueType() == MVT::i64 &&
           "Unexpected truncation");
    SDLoc DL(Base);
    SDValue Trunc = CurDAG->getNode(ISD::TRUNCATE, DL, VT, Base);
    insertDAGNode(CurDAG, Base.getNode(), Trunc);
    Base = Trunc;
  }

  // Lower the displacement to a TargetConstant.
  Offset = CurDAG->getTargetConstant(AM.Offset, SDLoc(Base), VT);
}


SDNode *XtensaDAGToDAGISel::splitLargeImmediate(unsigned Opcode, SDNode *Node,
                                                 SDValue Op0, uint64_t UpperVal,
                                                 uint64_t LowerVal) 
{
  EVT VT = Node->getValueType(0);
  SDLoc DL(Node);
  SDValue Upper = CurDAG->getConstant(UpperVal, DL, VT);
  if (Op0.getNode())
    Upper = CurDAG->getNode(Opcode, DL, VT, Op0, Upper);
#if 0
  Upper = SDValue(Select(Upper.getNode()), 0);

  SDValue Lower = CurDAG->getConstant(LowerVal, DL, VT);
  SDValue Or = CurDAG->getNode(Opcode, DL, VT, Upper, Lower);
  return Or.getNode();
#else
  {
    // When we haven't passed in Op0, Upper will be a constant. In order to
    // prevent folding back to the large immediate in `Or = getNode(...)` we run
    // SelectCode first and end up with an opaque machine node. This means that
    // we need to use a handle to keep track of Upper in case it gets CSE'd by
    // SelectCode.
    //
    // Note that in the case where Op0 is passed in we could just call
    // SelectCode(Upper) later, along with the SelectCode(Or), and avoid needing
    // the handle at all, but it's fine to do it here.
    //
    // TODO: This is a pretty hacky way to do this. Can we do something that
    // doesn't require a two paragraph explanation?
    HandleSDNode Handle(Upper);
    SelectCode(Upper.getNode());
    Upper = Handle.getValue();
  }

  SDValue Lower = CurDAG->getConstant(LowerVal, DL, VT);
  SDValue Or = CurDAG->getNode(Opcode, DL, VT, Upper, Lower);

  ReplaceUses(Node, Or.getNode());
  CurDAG->RemoveDeadNode(Node);

  SelectCode(Or.getNode());
  return 0;  // TODO
#endif
}

void XtensaDAGToDAGISel::Select(SDNode *Node) 
{
  SDLoc DL(Node);
  // Dump information about the Node being selected
  DEBUG(errs() << "Selecting: "; Node->dump(CurDAG); errs() << "\n");

  // If we have a custom node, we already have selected!
  if (Node->isMachineOpcode()) 
  {
    DEBUG(errs() << "== "; Node->dump(CurDAG); errs() << "\n");
    return;
  }

  unsigned Opcode = Node->getOpcode();
  switch (Opcode) 
  {
  case ISD::FrameIndex: 
  {
    SDValue imm = CurDAG->getTargetConstant(0, DL, MVT::i32);
    int FI = cast<FrameIndexSDNode>(Node)->getIndex();
    SDValue TFI =
        CurDAG->getTargetFrameIndex(FI, getTargetLowering()->getPointerTy(CurDAG->getDataLayout()));
    unsigned Opc = Xtensa::ADDI;
    EVT VT = MVT::i32;
    
    if(Node->hasOneUse()) //don't create a new node just morph this one
      //return CurDAG->SelectNodeTo(Node, Opc, VT, TFI, imm);
      CurDAG->SelectNodeTo(Node, Opc, VT, TFI, imm);
    //return CurDAG->getMachineNode(Opc, DL, VT, TFI, imm);
    CurDAG->getMachineNode(Opc, DL, VT, TFI, imm);
  }
  }//end special selections

  // Select the default instruction
#if 0
  SDNode *ResNode = SelectCode(Node);

  DEBUG(errs() << "=> ";
        if (ResNode == NULL || ResNode == Node)
          Node->dump(CurDAG);
        else
          ResNode->dump(CurDAG);
        errs() << "\n";
        );
  return ResNode;
#endif
  SelectCode(Node);
}

bool XtensaDAGToDAGISel::
SelectInlineAsmMemoryOperand(const SDValue &Op,
                             unsigned ConstraintID,
                             std::vector<SDValue> &OutOps) 
{
  switch(ConstraintID) 
  {
  default:
    llvm_unreachable("Unexpected asm memory constraint");
  case InlineAsm::Constraint_m:

    SDValue Base, Offset;
    selectMemRegAddr(Op, Base, Offset);
    OutOps.push_back(Base);
    OutOps.push_back(Offset);
    return false;
  }
  return false;
}

void XtensaDAGToDAGISel::processFunctionAfterISel(MachineFunction &MF) 
{
  /*for (auto &MBB: MF)
  {  
    for (auto &I: MBB) 
    {
      // TODO something useful for future
    }
  }  */
}
