{-# LANGUAGE TemplateHaskell, TypeFamilies, GeneralizedNewtypeDeriving, TypeSynonymInstances  #-}
module Punctuation where 
import qualified Data.Map as M
import Word
import POS
import NonTerm
import Text.PrettyPrint.HughesPJClass
import Sentence
import NLP.Probability.ConditionalDistribution
import TAG
import Data.DeriveTH
import Data.Binary hiding (Word)

mkPunctuationEvent = id
type PunctuationEvent = GWord 
--    deriving (Eq, Ord, Enum)

data PunctuationSubContext = PunctuationSubContext {
      headNT     :: Maybe NonTerm,
      parentNT   :: Maybe NonTerm,
      side       :: Maybe AdjunctionSide,
      atype      :: Maybe AdjunctionType, 
      parentPOS  :: Maybe POS, 
      parentWord :: Maybe Word,
      childNT    :: Maybe NonTerm,
      childWord  :: Maybe Word,
      childPOS   :: Maybe POS 
} deriving (Eq, Ord)

-- $( derive makeBinary ''PunctuationEvent )

newtype PunctuationContext = PunctuationContext [PunctuationSubContext]
    deriving (Eq, Ord, Show, Pretty)

puncSubDef = 
    PunctuationSubContext Nothing Nothing Nothing Nothing 
                          Nothing Nothing Nothing Nothing
                          Nothing

instance Context PunctuationContext where
    type Sub (PunctuationContext) = PunctuationSubContext
    type SubMap (PunctuationContext) = M.Map
    decompose (PunctuationContext pc) = pc 

pPrintJust Nothing = empty 
pPrintJust (Just p) = pPrint p

instance Show PunctuationSubContext where 
    show = render . pPrint

instance Pretty PunctuationSubContext where 
    pPrint s1 = 
        (pPrintJust $ headNT s1) <+>
        (pPrintJust $ atype s1) <+>
        (pPrintJust $ parentNT s1) <+>
        (pPrintJust $ side s1) <+>
        (pPrintJust $ parentPOS s1) <+>
        (pPrintJust $ parentWord s1) <+>
        (pPrintJust $ headNT s1) <+>
        (pPrintJust $ childNT s1) <+> 
        (pPrintJust $ childWord s1) <+>
        (pPrintJust $ childPOS s1) 
                

$( derive makeBinary ''PunctuationSubContext )

type PunctuationObservations = CondObserved PunctuationEvent PunctuationContext
type PunctuationDistribution = CondDistribution PunctuationEvent PunctuationContext
