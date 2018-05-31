//===----------------------------------------------------------------------===//
// Class definitions.
//===----------------------------------------------------------------------===//

class XtensaReg<string n> : Register<n> {
  let Namespace = "Xtensa";
}

class XtensaRegWithSubRegs<string n, list<Register> subregs>
  : RegisterWithSubRegs<n, subregs> {
  let Namespace = "Xtensa";
}

//===----------------------------------------------------------------------===//
// General-purpose registers
//===----------------------------------------------------------------------===//

// Xtensa general purpose regs
class ARReg<bits<4> num, string n> : XtensaReg<n> {
  field bits<4> number = num;
}

// Return Address
def a0 : ARReg<0, "a0">, DwarfRegNum<[0]>;

// Stack Pointer (callee-saved)
def sp : ARReg<1, "sp">, DwarfRegNum<[1]>;

// Function Arguments
def a2 : ARReg<2, "a2">, DwarfRegNum<[2]>;
def a3 : ARReg<3, "a3">, DwarfRegNum<[3]>;
def a4 : ARReg<4, "a4">, DwarfRegNum<[4]>;
def a5 : ARReg<5, "a5">, DwarfRegNum<[5]>;
def a6 : ARReg<6, "a6">, DwarfRegNum<[6]>;
def a7 : ARReg<7, "a7">, DwarfRegNum<[7]>;

// Static Chain
def a8 : ARReg<7, "a8">, DwarfRegNum<[8]>;

def a9 : ARReg<9, "a9">, DwarfRegNum<[9]>;
def a10 : ARReg<10, "a10">, DwarfRegNum<[10]>;
def a11 : ARReg<11, "a11">, DwarfRegNum<[11]>;

// Callee-saved
def a12 : ARReg<12, "a12">, DwarfRegNum<[12]>;
def a13 : ARReg<13, "a13">, DwarfRegNum<[13]>;
def a14 : ARReg<14, "a14">, DwarfRegNum<[14]>;

// Stack-Frame Pointer (optional) - Callee-Saved
def a15 : ARReg<15, "a15">, DwarfRegNum<[15]>;

//===----------------------------------------------------------------------===//
// Special-purpose registers
//===----------------------------------------------------------------------===//

//Xtensa special purpose regs
class SRReg<bits<8> num, string n> : XtensaReg<n> {
  field bits<8> number = num;
}

// Shift Amount Register
def sarl0 : SRReg<3, "sarl">;
def sarr0 : SRReg<3, "sarr">;

// Register Classes
def AR : RegisterClass<"Xtensa", [i32], 32, (add 
  a0, sp, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)>;

// Shift register SAR for left shifts
def SARL : RegisterClass<"Xtensa", [i32], 32, (add 
  sarl0)>;

// Shift register SAR for right shifts
def SARR : RegisterClass<"Xtensa", [i32], 32, (add 
  sarr0)>;
