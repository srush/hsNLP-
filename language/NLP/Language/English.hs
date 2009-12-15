{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, FlexibleInstances #-}
module NLP.Language.English where 

import NLP.Language
import qualified NLP.Language.English.NonTerm as NT
import qualified NLP.Language.English.POS as P
import qualified NLP.Language.Word as W
import Helpers.Common

newtype English = English () 

newtype EnglishWord = EnglishWord ()
instance W.Word EnglishWord where 
    getWordTable = W.wordTable "/tmp/engwords"

instance Show (Word English) where  show (EngWord e)  = show e
instance Show (NonTerm English) where  show (EngNT e)  = show e
instance Show (POS English) where  show (EngPOS e)  = show e

instance Language English where 
    newtype POS (English) = EngPOS P.POS 
                          deriving (Eq, Ord, Bounded, Enum, Arbitrary, Read)
    newtype NonTerm (English) = EngNT NT.NonTerm  
                          deriving (Eq, Ord, Bounded, Enum, Arbitrary, Read)
    newtype Word (English) = EngWord (W.WordWrap EnglishWord)
                          deriving (Eq, Ord, Bounded, Enum, Arbitrary)

    mkPOS = EngPOS . P.mkPOS
    mkNonTerm = EngNT . read
    mkWord = EngWord . W.mkWord
    isNP (EngNT NT.NP) = True
    isNP _ = False

    isNPB (EngNT NT.NPB) = True
    isNPB _ = False

    isVerb (EngPOS p) = P.isPOSVerb p  
    isComma (EngPOS p)= P.isPOSComma p 
    isPunc (EngPOS p) = P.isPOSPunc p 
