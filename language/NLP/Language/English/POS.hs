{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
module NLP.Language.English.POS where 
import Helpers.Common
import qualified Data.Bimap as BM
import qualified Data.Set as S

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
$( derive makeNFData ''POSCore )

newtype POS = POSWrap POSCore 
    deriving (Eq, Ord, Enum, Bounded, Binary, Arbitrary, Read, NFData)
             
instance Show POS where 
    show (POSWrap pcore) = fromMaybe (show pcore) $ BM.lookup pcore posMap

instance Pretty POS where pPrint = text . show  


--instance Read POS where 
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
