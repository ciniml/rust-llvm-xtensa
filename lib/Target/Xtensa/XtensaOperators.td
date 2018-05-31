//===----------------------------------------------------------------------===//
// Type profiles
//===----------------------------------------------------------------------===//
def SDT_XtensaCallSeqStart        : SDCallSeqStart<[SDTCisVT<0, i32>]>;
def SDT_XtensaCallSeqEnd          : SDCallSeqEnd<[SDTCisVT<0, i32>,
                                            SDTCisVT<1, i32>]>;
def SDT_XtensaCall               : SDTypeProfile<0, -1, [SDTCisPtrTy<0>]>;
def SDT_XtensaSelectCC           : SDTypeProfile<1, 3,
                                            [SDTCisSameAs<0, 1>,
                                             SDTCisSameAs<1, 2>,
                                             SDTCisVT<3, i32>]>;
def SDT_XtensaWrapPtr            : SDTypeProfile<1, 1,
                                            [SDTCisSameAs<0, 1>,
                                             SDTCisPtrTy<0>]>;
def SDT_XtensaFence              : SDTypeProfile<0, 2,[SDTCisVT<0, i32>,
                                                  SDTCisVT<1, i32>]>;

//===----------------------------------------------------------------------===//
// Node definitions
//===----------------------------------------------------------------------===//

def Xtensa_call: SDNode<"XtensaISD::CALL", SDT_XtensaCall,
                     [SDNPHasChain, SDNPOutGlue, SDNPOptInGlue, SDNPVariadic]>;

def Xtensa_callseq_start: SDNode<"ISD::CALLSEQ_START", SDT_XtensaCallSeqStart,
                                 [SDNPHasChain, SDNPSideEffect, SDNPOutGlue]>;
def Xtensa_callseq_end  : SDNode<"ISD::CALLSEQ_END",   SDT_XtensaCallSeqEnd,
                                 [SDNPHasChain, SDNPSideEffect, SDNPOptInGlue,
                                  SDNPOutGlue]>;

def Xtensa_retflag: SDNode<"XtensaISD::RET_FLAG", SDTNone,
                                 [SDNPHasChain, SDNPOptInGlue, SDNPVariadic]>;
def Xtensa_pcrel_wrapper: SDNode<"XtensaISD::PCREL_WRAPPER", SDT_XtensaWrapPtr, []>;
def Xtensa_select : SDNode<"XtensaISD::SELECT", SDTSelect>;
def Xtensa_select_cc: SDNode<"XtensaISD::SELECT_CC", SDT_XtensaSelectCC,
    		                 [SDNPInGlue]>;

def Xtensa_fence: SDNode<"XtensaISD::FENCE", SDT_XtensaFence, 
                 [SDNPHasChain, SDNPSideEffect]>;

