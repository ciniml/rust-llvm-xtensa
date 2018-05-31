include "XtensaOperators.td"

// Immediate operands with a shared generic render method.
class ImmAsmOperand<string name> : AsmOperandClass 
{ 
  let Name = name;
  let RenderMethod = "addImmOperands"; 
}

// Constructs both a DAG pattern and instruction operand for an immediate
// of type VT.  PRED returns true if a node is acceptable and XFORM returns
// the operand value associated with the node.  ASMOP is the name of the
// associated asm operand, and also forms the basis of the asm print method.
class Immediate<ValueType vt, code pred, string asmop>
  : Operand<vt>, ImmLeaf<vt, pred> 
{
  let PrintMethod = "print"##asmop;
  let ParserMatchClass = !cast<AsmOperandClass>(asmop);
}

// imm8 predicate - Immediate in the range [-128,127]
def Imm8_AsmOperand: ImmAsmOperand<"imm8">;
def imm8: Immediate<i32, [{ return Imm >= -128 && Imm <= 127; }], "Imm8_AsmOperand">; 

// imm7n predicate - Immediate in the range [-32,95]
def Imm7n_AsmOperand: ImmAsmOperand<"imm7n">;
def imm7n: Immediate<i32, [{ return Imm >= -32 && Imm <= 95; }], "Imm7n_AsmOperand">; 

// imm12 predicate - Immediate in the range [-2048,2047]
def Imm12_AsmOperand: ImmAsmOperand<"imm12">;
def imm12: Immediate<i32, [{ return Imm >= -2048 && Imm <= 2047; }], "Imm12_AsmOperand">; 

// immn predicate - Narrow Immediate in the range [-1,15]
def Immn_AsmOperand: ImmAsmOperand<"immn">;
def immn: Immediate<i32, [{ return Imm >= -1 && Imm <= 15; }], "Immn_AsmOperand">; 

// shimm4 predicate - Immediate in the range [0,15]
def Shimm4_AsmOperand: ImmAsmOperand<"shimm4">;
def shimm4: Immediate<i32, [{ return Imm >= 0 && Imm <= 15; }], "Shimm4_AsmOperand">; 

// shimm5 predicate - Immediate in the range [0,31]
def Shimm5_AsmOperand: ImmAsmOperand<"shimm5">;
def shimm5: Immediate<i32, [{ return Imm >= 0 && Imm <= 31; }], "Shimm5_AsmOperand">; 

// Memory offset 0..255 for 8-bit memory accesses
def Offset8m8_AsmOperand: ImmAsmOperand<"offset8m8">;
def offset8m8: Immediate<i32, 
  [{ return Imm >= 0 && Imm <= 255; }], 
  "Offset8m8_AsmOperand">;

// Memory offset 0..510 for 16-bit memory accesses
def Offset8m16_AsmOperand: ImmAsmOperand<"offset8m16">;
def offset8m16: Immediate<i32, 
  [{ return Imm >= 0 && Imm <= 510 && (Imm & 0x1 == 0); }], 
  "Offset8m16_AsmOperand">;

// Memory offset 0..1020 for 32-bit memory accesses
def Offset8m32_AsmOperand: ImmAsmOperand<"offset8m32">;
def offset8m32: Immediate<i32, 
  [{ return Imm >= 0 && Imm <= 1020 && (Imm & 0x3 == 0); }], 
  "Offset8m32_AsmOperand">;

// Memory offset 0..60 for 32-bit memory accesses
def Offset4m32_AsmOperand: ImmAsmOperand<"offset4m32">;
def offset4m32: Immediate<i32, 
  [{ return Imm >= 0 && Imm <= 60 && (Imm & 0x3 == 0); }], 
  "Offset4m32_AsmOperand">;

def imm32 : ImmLeaf<i32, [{}]>, Operand<i32>;

//===----------------------------------------------------------------------===//
// Memory address operands
//===----------------------------------------------------------------------===//

class mem<Operand offset> : Operand<i32> 
{
  let MIOperandInfo = (ops offset, AR);
  //let EncoderMethod = "getMemRegEncoding";
  let OperandType = "OPERAND_MEMORY";
  let PrintMethod = "printMemOperand";
}

def mem8: mem<offset8m8>;
def mem16: mem<offset8m16>;
def mem32: mem<offset8m32>;
def mem32n: mem<offset4m32>;

def addr: ComplexPattern<iPTR, 2, "selectMemRegAddr">;

//===----------------------------------------------------------------------===//
// Symbolic address operands
//===----------------------------------------------------------------------===//

class PCRelAddress<ValueType vt, string self>
  : ComplexPattern<vt, 1, "selectPCRelAddress", [Xtensa_pcrel_wrapper]>,
    Operand<vt> 
{
  let MIOperandInfo = (ops !cast<Operand>(self));
}

def pcrel32call: PCRelAddress<i32, "pcrel32call"> 
{
  let PrintMethod = "printCallOperand";
  let EncoderMethod = "getCallEncoding";
}

def brtarget : Operand<OtherVT> 
{
  let PrintMethod = "printBranchTarget";
  let EncoderMethod = "getBranchTargetEncoding";
}

def jumptarget: Operand<OtherVT> 
{
  let PrintMethod = "printJumpTarget";
  let EncoderMethod = "getJumpTargetEncoding";
}

