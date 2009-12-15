{-# LANGUAGE TemplateHaskell #-}
module NLP.Language.English.NonTerm where 
import Helpers.Common

data NonTerm =  ADJP | ADJP_CC | ADVP | ADVP_CC | CONJP | FRAG | FRAG_CC | INTJ | INTJ_CC | LST | NAC | NAC_CC | NP | NP_CC | NX | NX_CC | PP | PP_CC | PRN | PRN_CC | PRT | PRT_CC | QP | QP_CC | RRC | RRC_CC | S | SBAR | SBARQ | SBARQ_CC | SBAR_CC | SINV | SINV_CC | SQ | SQ_CC | S_CC | UCP | UCP_CC | VP | VP_CC | WHADJP | WHADJP_CC | WHADVP | WHADVP_CC | WHNP | WHNP_CC | WHPP | X | X_CC | NPB | ROOT 
    deriving (Read, Show, Eq, Ord, Enum, Bounded)

$( derive makeBinary ''NonTerm )
$( derive makeArbitrary ''NonTerm )
