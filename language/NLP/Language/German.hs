{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving #-}
module NLP.Language.German where 

import NLP.Language
import qualified NLP.Language.German.NonTerm as NT
import qualified NLP.Language.German.POS as P
import qualified NLP.Language.Word as W
import qualified NLP.Language.English.POS as E
import Helpers.Common

newtype German = German () 

newtype GermanWord = GermanWord ()
instance W.Word GermanWord where 
    getWordTable = W.wordTable "/tmp/gerwords"


instance Language German where 
    newtype POS (German) = GerPOS P.POS 
                          deriving (Eq, Ord, Show, Bounded, Enum, Arbitrary, Read)
    newtype NonTerm (German) = GerNT NT.NonTerm
                          deriving (Eq, Ord, Show, Bounded, Enum, Arbitrary, Read)
    newtype Word (German) = GerWord (W.WordWrap GermanWord)
                          deriving (Eq, Ord, Show, Bounded, Enum, Arbitrary)

    mkPOS = GerPOS . P.mkPOS
    mkNonTerm = GerNT . read
    mkWord = GerWord . W.mkWord
    isNP (GerNT NT.NP) = True
    isNP _ = False

    isNPB (GerNT NT.NPB) = True
    isNPB _ = False

    isVerb (GerPOS p) = E.isPOSVerb $ P.toJoint  p  
    isComma (GerPOS p)= E.isPOSComma $ P.toJoint  p  
    isPunc (GerPOS p) = E.isPOSPunc $ P.toJoint  p  

