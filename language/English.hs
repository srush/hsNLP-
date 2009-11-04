module NLP.Language.English where 

data NonTerm =  ADJP | ADJP_CC | ADVP | ADVP_CC | CONJP | FRAG | FRAG_CC | INTJ | INTJ_CC | LST | NAC | NAC_CC | NP | NP_CC | NX | NX_CC | PP | PP_CC | PRN | PRN_CC | PRT | PRT_CC | QP | QP_CC | RRC | RRC_CC | S | SBAR | SBARQ | SBARQ_CC | SBAR_CC | SINV | SINV_CC | SQ | SQ_CC | S_CC | UCP | UCP_CC | VP | VP_CC | WHADJP | WHADJP_CC | WHADVP | WHADVP_CC | WHNP | WHNP_CC | WHPP | X | X_CC | NPB | ROOT 
    deriving (Read, Show, Eq, Ord, Enum, Bounded)

$( derive makeBinary ''PureNonTerm )
$( derive makeArbitrary ''PureNonTerm )


instance Language(English) where 
    data POS (English) = POS 
    data NonTerm(English) = NonTerm
    isNP NP = True
    isNP _ = False

    isNPB NPB = True
    isNPB _ = False


-- These are special symbols where the string rep is different than the 
-- constructor
posMap = BM.fromList [(NUM, "#"),
                      (DOL, "$"),
                      (QUOT, "''"),
                      (COM, ","),
                      (LRB, "-LRB-"),
                      (RRB, "-RRB-"), 
                      (DOT, "."),
                      (COL, ":"),
                      (WPDOL, "WP$"),
                      (BQUOT, "``"), 
                      (PRPDOL, "PRP$") ]


data POSCore = NUM | DOL | QUOT | COM | LRB | RRB | DOT | COL | CC | CD | DT | EX | FW | IN | JJ | JJR | JJS | LS | MD | NN | NNP | NNPS | NNS | PDT | POS | PRP | PRPDOL | RB | RBR | RBS | RP | SYM | TO | UH | VB | VBD | VBG | VBN | VBP | VBZ | WDT | WP | WPDOL | WRB | BQUOT | ROOT
    deriving (Eq, Ord, Enum, Bounded, Show, Read)

$( derive makeBinary ''POSCore )
$( derive makeArbitrary ''POSCore )

newtype POS = POSWrap POSCore 
    deriving (Eq, Ord, Enum, Bounded, Binary, Arbitrary, Read)
             
instance Show POS where 
    show (POSWrap pcore) = fromMaybe (show pcore) $ BM.lookup pcore posMap

instance Pretty POS where pPrint = text . show  

mkPOS :: String -> POS
mkPOS str = POSWrap $ fromMaybe ( readNote ("POS " ++ str) str) $ BM.lookupR str posMap

verbSet = S.fromList [VB, VBD, VBG, VBN, VBP, VBZ]
isPOSVerb (POSWrap pos) = S.member pos verbSet 

commaSet = S.fromList [COM, COL]
isPOSComma (POSWrap pos) = S.member pos commaSet 

puncSet = S.fromList [DOT, QUOT, BQUOT]
isPOSPunc (POSWrap pos) = S.member pos puncSet 

conjSet = S.fromList [CC]
isPOSConj (POSWrap pos) =  S.member pos conjSet

posSet = S.fromList [POS]
isPOSPossessive (POSWrap pos) =  S.member pos posSet
