include "XtensaInstrFormats.td"
include "XtensaOperands.td"

// Arithmetic & Logical instructions
class ArithLogic_RRRN<bits<4> oper0, string instrAsm, SDPatternOperator opNode, 
    bit isComm = 0>:
  RRRN_Inst<oper0, (outs AR:$r), (ins AR:$s, AR:$t),
     instrAsm#"\t$r, $s, $t",
     [(set AR:$r, (opNode AR:$s, AR:$t))]> 
{
  let isCommutable = isComm;
  let isReMaterializable = 0;
}

def ADD_N: ArithLogic_RRRN<0x08, "add.n", add, 1>;

class ArithLogic_RRR<bits<4> oper2, bits<4> oper1, string instrAsm, 
    SDPatternOperator opNode, bit isComm = 0>:
  RRR_Inst<0x00, oper1, oper2, (outs AR:$r), (ins AR:$s, AR:$t),
     instrAsm#"\t$r, $s, $t",
     [(set AR:$r, (opNode AR:$s, AR:$t))]> 
{
  let isCommutable = isComm;
  let isReMaterializable = 0;
}

def ADD: ArithLogic_RRR<0x08, 0x00, "add", add, 1>;
def SUB: ArithLogic_RRR<0x0C, 0x00, "sub", sub>;
def AND: ArithLogic_RRR<0x01, 0x00, "and", and, 1>;
def MULL: ArithLogic_RRR<0x08, 0x02, "mull", mul, 1>;
def QUOS: ArithLogic_RRR<0x0D, 0x02, "quos", sdiv>;
def QUOU: ArithLogic_RRR<0x0C, 0x02, "quou", udiv>;
def REMS: ArithLogic_RRR<0x0F, 0x02, "quos", srem>;
def REMU: ArithLogic_RRR<0x0E, 0x02, "quou", urem>;
def OR: ArithLogic_RRR<0x02, 0x00, "or", or, 1>;
def XOR: ArithLogic_RRR<0x03, 0x00, "xor", xor, 1>;

def ADDI_N: RRRN_Inst<0x0A, (outs AR:$t), (ins AR:$s, immn:$imm),
     "addi\t$t, $s, $imm",
     [(set AR:$t, (add AR:$s, immn:$imm))]> 
{
//  let r = imm + 1;
}

def ADDI: RRI8_Inst<0x02, (outs AR:$t), (ins AR:$s, imm8:$imm8),
     "addi\t$t, $s, $imm8",
     [(set AR:$t, (add AR:$s, imm8:$imm8))]> 
{
  let r = 0x0C;
}

class ADDX<bits<4> oper, string instrAsm, list<dag> pattern>:
  RRR_Inst<0x00, 0x00, oper, (outs AR:$r), (ins AR:$s, AR:$t),
     instrAsm#"\t$r, $s, $t",
     pattern>; 

def ADDX2: ADDX<0x09, "addx2", [(set AR:$r, (add AR:$s, (shl AR:$t, (i32 1))))]>;
def ADDX4: ADDX<0x0A, "addx4", [(set AR:$r, (add AR:$s, (shl AR:$t, (i32 2))))]>;
def ADDX8: ADDX<0x0B, "addx8", [(set AR:$r, (add AR:$s, (shl AR:$t, (i32 3))))]>;
def SUBX2: ADDX<0x0D, "subx2", [(set AR:$r, (sub (shl AR:$s, (i32 1)), AR:$t))]>;
def SUBX4: ADDX<0x0E, "subx4", [(set AR:$r, (sub (shl AR:$s, (i32 2)), AR:$t))]>;
def SUBX8: ADDX<0x0F, "subx8", [(set AR:$r, (sub (shl AR:$s, (i32 3)), AR:$t))]>;

def NEG: RRR_Inst<0x00, 0x00, 0x06, (outs AR:$r), (ins AR:$t),
     "neg\t$r, $t",
		 [(set AR:$r, (ineg AR:$t))]>;

// Shift instructions
def SLL: RRR_Inst<0x00, 0x01, 0x0A, (outs AR:$r), (ins AR:$s, SARL:$sa),
     "sll\t$r, $s",
     [(set AR:$r, (shl AR:$s, SARL:$sa))]>;

def SRA: RRR_Inst<0x00, 0x01, 0x0B, (outs AR:$r), (ins AR:$t, SARR:$sa),
     "sra\t$r, $t",
     [(set AR:$r, (sra AR:$t, SARR:$sa))]>;

def SRL: RRR_Inst<0x00, 0x01, 0x09, (outs AR:$r), (ins AR:$t, SARR:$sa),
     "srl\t$r, $t",
     [(set AR:$r, (sra AR:$t, SARR:$sa))]>;

def SSL: RRR_Inst<0x00, 0x00, 0x04, (outs SARL:$sa), (ins AR:$s),
     "ssl\t$s", [(set SARL:$sa, AR:$s)]>
{
  let r = 0x01;
}
def SSR: RRR_Inst<0x00, 0x00, 0x04, (outs SARR:$sa), (ins AR:$s),
     "ssr\t$s", [(set SARR:$sa, AR:$s)]>
{
  let r = 0x00;
}

def SLLI: RRR_Inst<0x00, 0x01, 0x00, (outs AR:$r), (ins AR:$s, shimm5:$sa),
     "slli\t$r, $s, $sa",
     [(set AR:$r, (shl AR:$s, shimm5:$sa))]> 
{
  field bits<5> sa;

  let op2 = !and(!srl(sa, 4), 0x01);
  let t = !and(sa, 0xF);
} 

def SRAI: RRR_Inst<0x00, 0x01, 0x02, (outs AR:$r), (ins AR:$t, shimm5:$sa),
     "srai\t$r, $t, $sa",
     [(set AR:$r, (shl AR:$t, shimm5:$sa))]> 
{
  bits<5> sa;

  let op2 = !add(0x02, !and(!srl(sa, 4), 0x01));
  let s = !and(sa, 0xF);
}

def SRLI: RRR_Inst<0x00, 0x01, 0x02, (outs AR:$r), (ins AR:$t, shimm4:$sa),
     "srli\t$r, $t, $sa",
     [(set AR:$r, (shl AR:$t, shimm4:$sa))]> 
{
  bits<4> sa; 

  let s = sa;
}  

// Move instructions
def MOV_N: RRRN_Inst<0x0D, (outs AR:$t), (ins AR:$s),
   "mov.n\t$t, $s",
   [(set AR:$t, AR:$s)]>
{
  let r = 0;
}

def MOVI_N: RI7_Inst<0x02, (outs AR:$s), (ins imm7n:$imm7),
   "movi.n\t$s, $imm7",
   [(set AR:$s, imm7n:$imm7)]>;

def MOVI: RRI8_Inst<0x02, (outs AR:$t), (ins imm12:$imm),
   "movi\t$t, $imm",
   [(set AR:$t, imm12:$imm)]>
{
  bits<12> imm;

  let imm8 = !and(imm, 0xFF);
  let s = !srl(imm, 8);
}

//TODO replace to L32R
def LI : XtensaInst<4, (outs AR:$dst), (ins imm32:$imm), "li\t$dst, $imm",
  []>
{
    let isPseudo = 1;
}

// Set Shift Amount register
def SSAI: RRR_Inst<0x00, 0x00, 0x04, (outs SARR:$sar), (ins shimm5:$imm),
     "ssai\t$imm",
     [(set SARR:$sar, shimm5:$imm)]>
{
  bits<5> imm;

  let r = 0x04;
  let s = !and(imm, 0xF);
  let t = !srl(imm, 4);
}

// Load instructions
let mayLoad = 1 in
{
  def L32I_N: RRRN_Inst<0x8, (outs AR:$t), (ins mem32n:$addr), 
                "l32i.n\t$t, $addr", 
                [(set AR:$t, (load addr:$addr))]>;

  class Load_RRI8<bits<4> oper, string instrAsm, SDPatternOperator opNode, 
      Operand memOp>: RRI8_Inst<0x02, (outs AR:$t), (ins memOp:$addr), 
                instrAsm#"\t\t$t, $addr", 
                [(set AR:$t, (opNode addr:$addr))]>
  {
    let r = oper;
  }
}

def L8UI: Load_RRI8<0x09, "l8ui", sextloadi8, mem8>;
def L16SI: Load_RRI8<0x09, "l16si", sextloadi16, mem16>;
def L16UI: Load_RRI8<0x01, "l16ui", zextloadi16, mem16>;
def L32I: Load_RRI8<0x02, "l32i", load, mem32>;

//extended loads
def : Pat<(i32 (extloadi1  addr:$addr)), (L8UI addr:$addr)>;
def : Pat<(i32 (extloadi8  addr:$addr)), (L8UI addr:$addr)>;
def : Pat<(i32 (extloadi16 addr:$addr)), (L16UI addr:$addr)>;

// Store instructions
let mayStore = 1 in
{
  def S32I_N: RRRN_Inst<0x8, (outs), (ins AR:$t, mem32n:$addr),
              "s32.n\t$t, $addr", 
              [(store AR:$t, addr:$addr)]>;

  class Store_II8<bits<4> oper, string instrAsm, SDPatternOperator opNode, 
      Operand memOp>: RRI8_Inst<0x02, (outs), (ins AR:$t, memOp:$addr),
              instrAsm#"\t\t$t, $addr", 
              [(opNode AR:$t, addr:$addr)]>
  {
    let r = oper;
  }

}

def S8I: Store_II8<0x04, "s8i", truncstorei8, mem8>;
def S16I: Store_II8<0x05, "s16i", truncstorei16, mem16>;
def S32I: Store_II8<0x06, "s32i", store, mem32>;
 
def L32R: RI16_Inst<0x01, (outs AR:$t), (ins imm32:$label), "l32r\t$t, $label", 
    []>;

// Conditional move instructions
def MOVEQZ : RRR_Inst<0x00, 0x03, 0x08, (outs AR:$r), (ins AR:$s, AR:$t),
     "moveqz\t$r, $s, $t",
     []>;
def MOVNEZ : RRR_Inst<0x00, 0x03, 0x09, (outs AR:$r), (ins AR:$s, AR:$t),
     "movnez\t$r, $s, $t",
     []>;
def MOVLTZ : RRR_Inst<0x00, 0x03, 0x0A, (outs AR:$r), (ins AR:$s, AR:$t),
     "movltz\t$r, $s, $t",
     []>;
def MOVGEZ : RRR_Inst<0x00, 0x03, 0x0B, (outs AR:$r), (ins AR:$s, AR:$t),
     "movgez\t$r, $s, $t",
     []>;

// LA is pseudo instruction for loading address from literal pool by L32R
let usesCustomInserter = 1 in
{
  def LA: Pseudo<(outs AR:$dst), (ins imm32:$label),
                       "# LA PSEUDO!", []>;
}

// Conditional branch instructions
let isBranch = 1, isTerminator = 1, isBarrier = 1 in 
{
  class Branch_RR<bits<4> oper, string instrAsm, SDPatternOperator opNode>: 
        RRI8_Inst<0x07, (outs), 
              (ins brtarget:$target, AR:$s, AR:$t), 
              instrAsm#"\t$s, $t, $target", 
              [(brcond (i32 (opNode AR:$s, AR:$t)), bb:$target)]>
  {
    let r = oper;
  }

  class Branch_RR_rev<bits<4> oper, string instrAsm, SDPatternOperator opNode>: 
        RRI8_Inst<0x07, (outs), 
              (ins brtarget:$target, AR:$s, AR:$t), 
              instrAsm#"\t$t, $s, $target", 
              [(brcond (i32 (opNode AR:$s, AR:$t)), bb:$target)]>
  {
    let r = oper;
  }

  class Branch_RI<bits<4> oper, string instrAsm, SDPatternOperator opNode>: 
        RRI8_Inst<0x06, (outs), 
              (ins brtarget:$target, AR:$s, imm8:$imm), 
              instrAsm#"\t$s, $imm, $target", 
              [(brcond (i32 (opNode AR:$s, imm8:$imm)), bb:$target)]>
  {
    let t = oper;
  }

  class Branch_RZ<bits<4> oper, string instrAsm, SDPatternOperator opNode>: 
        RRI8_Inst<0x06, (outs), 
              (ins brtarget:$target, AR:$s), 
              instrAsm#"\t$s, $target", 
              [(brcond (i32 (opNode AR:$s, (i32 0))), bb:$target)]>
  {
    let r = oper;
  }
}

def BEQ: Branch_RR<0x01, "beq", seteq>;
def BNE: Branch_RR<0x09, "bne", setne>;
def BGE: Branch_RR<0x0A, "bge", setge>;
def BLT: Branch_RR<0x02, "blt", setlt>;
def BGEU: Branch_RR<0x0B, "bgeu", setuge>;
def BLTU: Branch_RR<0x03, "bltu", setult>;

//Synthesize remaining condition codes by reverseing operands
def BGT: Branch_RR_rev<0x02, "blt", setgt>;
def BGTU: Branch_RR_rev<0x03, "bltu", setugt>;
def BLE: Branch_RR_rev<0x0A, "bge", setle>;
def BLEU: Branch_RR_rev<0x0B, "bgeu", setule>;

def BEQI: Branch_RI<0x02, "beqi", seteq>;
def BNEI: Branch_RI<0x06, "beqi", setne>;
def BGEI: Branch_RI<0x0E, "bgei", setge>;
def BLTI: Branch_RI<0x0A, "blti", setlt>;
def BGEUI: Branch_RI<0x0F, "bgeui", setuge>;
def BLTUI: Branch_RI<0x0B, "bltui", setult>;

def BEQZ: Branch_RZ<0x01, "beqz", seteq>;
def BNEZ: Branch_RZ<0x05, "bnez", setne>;
def BGEZ: Branch_RZ<0x0D, "bgez", setge>;
def BLTZ: Branch_RZ<0x09, "bltz", setlt>;

// Unconditional jumps
let isBranch = 1, isTerminator = 1, isBarrier = 1 in 
{
  def J: CALL_Inst<0x06, (outs), (ins jumptarget:$offset), "j\t$offset", 
          [(br bb:$offset)]>;

  def JX: CALLX_Inst<0x00, 0x00, 0x00, (outs), (ins AR:$s), "jx\t$s",
          [(brind AR:$s)]>
  {
    let m = 0x2;
    let n = 0x2;
    let isIndirectBranch = 1;
  }
}

let isCall = 1, isBranch = 1, isTerminator = 1, isBarrier = 1, 
   Defs = [a0, a2, a3, a4, a5] in 
{

  def CALL0: CALL_Inst<0x05, (outs), (ins pcrel32call:$offset),
          "call0\t$offset", 
          [(Xtensa_call pcrel32call:$offset)]>;

  def CALLX0: CALLX_Inst<0x00, 0x00, 0x00, (outs), (ins AR:$s), 
          "callx0\t$s", [(Xtensa_call AR:$s)]>
  {
    let m = 0x2;
    let n = 0x0;
    let isIndirectBranch = 1;
  }
}

let isReturn = 1, isTerminator = 1, isCodeGenOnly = 1,
    isBarrier = 1, hasCtrlDep = 1 in
{

  def RET_N: RRRN_Inst<0x00, (outs), (ins),
                "ret.n", [(Xtensa_retflag)]>
  {
    let r = 0x0F;
    let s = 0;
    let t = 0;
  } 
/*
  def RET: CALLX_Inst<0x00, 0x00, 0x00, (outs), (ins),
                "ret", [(Xtensa_retflag)]>
  {
    let m = 0x2;
    let n = 0x0;
  }
*/
}

def : Pat<(Xtensa_retflag), (RET_N)>;

// calls
def : Pat<(Xtensa_call (i32 tglobaladdr:$dst)),
          (CALL0 tglobaladdr:$dst)>;
def : Pat<(Xtensa_call (i32 texternalsym:$dst)),
          (CALLX0 texternalsym:$dst)>;

//pcrel addr loading using L32R
def : Pat<(Xtensa_pcrel_wrapper tglobaladdr:$in), (LA tglobaladdr:$in)>;
def : Pat<(Xtensa_pcrel_wrapper tblockaddress:$in), (LA tblockaddress:$in)>;
def : Pat<(Xtensa_pcrel_wrapper tjumptable:$in), (LA tjumptable:$in)>;
def : Pat<(Xtensa_pcrel_wrapper tconstpool:$in), (LA tconstpool:$in)>;
def : Pat<(Xtensa_pcrel_wrapper tglobaltlsaddr:$in), (LA tglobaltlsaddr:$in)>;
def : Pat<(Xtensa_pcrel_wrapper texternalsym:$in), (LA texternalsym:$in)>;

//===----------------------------------------------------------------------===//
// Stack allocation
//===----------------------------------------------------------------------===//

// ADJCALLSTACKDOWN/UP implicitly use/def SP because they may be expanded into
// a stack adjustment and the codegen must know that they may modify the stack
// pointer before prolog-epilog rewriting occurs.
let Defs = [sp], Uses = [sp] in 
{
def ADJCALLSTACKDOWN: Pseudo<(outs), (ins i32imm:$amt1, i32imm:$amt2), 
                              "#ADJCALLSTACKDOWN",
                              [(Xtensa_callseq_start timm:$amt1, timm:$amt2)]>;
def ADJCALLSTACKUP  : Pseudo<(outs), (ins i32imm:$amt1, i32imm:$amt2),
                              "#ADJCALLSTACKUP",
                              [(Xtensa_callseq_end timm:$amt1, timm:$amt2)]>;
}

// Generic select instruction
let usesCustomInserter = 1 in
{
//def SELECT   : Pseudo<(outs AR:$res), (ins AR:$a, AR:$x, AR:$y),
//                       "!select $res, $a, $x, $y",
//                       [(set AR:$res, (Xtensa_select AR:$a, AR:$x, AR:$y))]>;

// Expanded into a branch sequence.
def SELECT_CC : Pseudo<(outs AR:$dst),
                       (ins AR:$cond, AR:$t, AR:$f),
                        "# SELECT_CC PSEUDO!",
                       [(set AR:$dst,
                       (select AR:$cond, AR:$t, AR:$f))]>;

}
def : Pat<(select (i32 (setne AR:$lhs, 0)), AR:$t, AR:$f),
        (SELECT_CC AR:$lhs, AR:$t, AR:$f)>;
  
def : Pat<(select (i32 (seteq AR:$lhs, 0)), AR:$t, AR:$f),
        (SELECT_CC AR:$lhs, AR:$f, AR:$t)>;
