//===----------------------------------------------------------------------===//
// Type profiles
//===----------------------------------------------------------------------===//
def SDT_XtensaCallSeqStart        : SDCallSeqStart<[SDTCisVT<0, i32>, SDTCisVT<1, i32>]>;
def SDT_XtensaCallSeqEnd          : SDCallSeqEnd<[SDTCisVT<0, i32>,
                                            SDTCisVT<1, i32>]>;
def SDT_XtensaCall               : SDTypeProfile<0, -1, [SDTCisPtrTy<0>]>;
def SDT_XtensaSelectCC           : SDTypeProfile<1, 5,
                                            [SDTCisSameAs<0, 1>,
                                             SDTCisSameAs<2, 3>, 
                                             SDTCisVT<5, i32>]>;
def SDT_XtensaWrapPtr            : SDTypeProfile<1, 1,
                                            [SDTCisSameAs<0, 1>,
                                             SDTCisPtrTy<0>]>;
def SDT_XtensaFence              : SDTypeProfile<0, 2,[SDTCisVT<0, i32>,
                                                  SDTCisVT<1, i32>]>;
def SDT_XtensaCmp                : SDTypeProfile<1, 2, [SDTCisVT<0, i1>, SDTCisVT<1, f32>, SDTCisVT<2, f32>]>;
def SDT_XtensaBrCC               : SDTypeProfile<0, 2, [SDTCisVT<0, i1>, SDTCisVT<1, OtherVT>]>;
def SDT_XtensaSelectCCFP         : SDTypeProfile<1, 5, [SDTCisSameAs<0, 3>, SDTCisSameAs<1, 2>, SDTCisSameAs<3, 4>, SDTCisVT<1, f32>, SDTCisVT<5, i32>]>;
def SDT_XtensaMoveCond           : SDTypeProfile<1, 1, [SDTCisSameAs<0, 1>, SDTCisVT<0, i32>]>;

def SDT_XtensaMADD               : SDTypeProfile<1, 3, [SDTCisSameAs<0, 1>, SDTCisSameAs<0, 2>, SDTCisSameAs<0, 3>, SDTCisVT<0, f32>]>;
def SDT_XtensaMOVS               : SDTypeProfile<1, 1, [SDTCisSameAs<0, 1>, SDTCisVT<0, f32>]>;

//===----------------------------------------------------------------------===//
// Node definitions
//===----------------------------------------------------------------------===//

def Xtensa_call: SDNode<"XtensaISD::CALL", SDT_XtensaCall,
                     [SDNPHasChain, SDNPOutGlue, SDNPOptInGlue, SDNPVariadic]>;
def Xtensa_callw: SDNode<"XtensaISD::CALLW", SDT_XtensaCall,
                     [SDNPHasChain, SDNPOutGlue, SDNPOptInGlue, SDNPVariadic]>;

def Xtensa_callseq_start: SDNode<"ISD::CALLSEQ_START", SDT_XtensaCallSeqStart,
                                 [SDNPHasChain, SDNPSideEffect, SDNPOutGlue]>;
def Xtensa_callseq_end  : SDNode<"ISD::CALLSEQ_END",   SDT_XtensaCallSeqEnd,
                                 [SDNPHasChain, SDNPSideEffect, SDNPOptInGlue,
                                  SDNPOutGlue]>;

def Xtensa_retflag: SDNode<"XtensaISD::RET_FLAG", SDTNone,
                                 [SDNPHasChain, SDNPOptInGlue, SDNPVariadic]>;
def Xtensa_retWflag: SDNode<"XtensaISD::RETW_FLAG", SDTNone,
                                 [SDNPHasChain, SDNPOptInGlue, SDNPVariadic]>;
def Xtensa_pcrel_wrapper: SDNode<"XtensaISD::PCREL_WRAPPER", SDT_XtensaWrapPtr, []>;
def Xtensa_select : SDNode<"XtensaISD::SELECT", SDTSelect>;
def Xtensa_select_cc: SDNode<"XtensaISD::SELECT_CC", SDT_XtensaSelectCC,
    		                 [SDNPInGlue]>;

def Xtensa_fence: SDNode<"XtensaISD::FENCE", SDT_XtensaFence, 
                 [SDNPHasChain, SDNPSideEffect]>;

def Xtensa_cmpoeq     : SDNode<"XtensaISD::CMPOEQ", SDT_XtensaCmp, [SDNPOutGlue]>;
def Xtensa_cmpolt     : SDNode<"XtensaISD::CMPOLT", SDT_XtensaCmp, [SDNPOutGlue]>;
def Xtensa_cmpole     : SDNode<"XtensaISD::CMPOLE", SDT_XtensaCmp, [SDNPOutGlue]>;
def Xtensa_cmpueq    : SDNode<"XtensaISD::CMPUEQ", SDT_XtensaCmp, [SDNPOutGlue]>;
def Xtensa_cmpult    : SDNode<"XtensaISD::CMPULT", SDT_XtensaCmp, [SDNPOutGlue]>;
def Xtensa_cmpule    : SDNode<"XtensaISD::CMPULE", SDT_XtensaCmp, [SDNPOutGlue]>;
def Xtensa_cmpuo     : SDNode<"XtensaISD::CMPUO", SDT_XtensaCmp, [SDNPOutGlue]>;

def Xtensa_brcc      : SDNode<"XtensaISD::BR_CC", SDT_XtensaBrCC,
                            [SDNPHasChain, SDNPInGlue]>;
def Xtensa_brcc_t    : SDNode<"XtensaISD::BR_CC_T", SDT_XtensaBrCC,
                            [SDNPHasChain, SDNPInGlue]>;
def Xtensa_brcc_f    : SDNode<"XtensaISD::BR_CC_F", SDT_XtensaBrCC,
                            [SDNPHasChain, SDNPInGlue]>;

def Xtensa_select_cc_fp: SDNode<"XtensaISD::SELECT_CC_FP", SDT_XtensaSelectCCFP,
                            [SDNPInGlue]>;

def Xtensa_movt: SDNode<"XtensaISD::MOVT", SDT_XtensaMoveCond,
                            [SDNPInGlue]>;
def Xtensa_movf: SDNode<"XtensaISD::MOVF", SDT_XtensaMoveCond,
                            [SDNPInGlue]>;

def Xtensa_madd: SDNode<"XtensaISD::MADD", SDT_XtensaMADD,
                            [SDNPInGlue]>;
def Xtensa_msub: SDNode<"XtensaISD::MSUB", SDT_XtensaMADD,
                            [SDNPInGlue]>;
def Xtensa_movs: SDNode<"XtensaISD::MOVS", SDT_XtensaMOVS,
                            [SDNPInGlue]>;
