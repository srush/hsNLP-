{-# LANGUAGE  GeneralizedNewtypeDeriving, TemplateHaskell #-}
module POS (mkPOS, isPOSVerb, isPOSPunc, isPOSComma, isPOSConj, POS)  where 
import qualified Data.Bimap as BM
import qualified Data.Set as S
import Common

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