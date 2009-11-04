{-# LANGUAGE TypeFamilies #-}
module NLP.Language.English where 

import NLP.Language
import qualified NLP.Language.English.NonTerm as NT
import qualified NLP.Language.English.POS as P
import qualified NLP.Language.Word as W

newtype English = English () 

newtype EnglishWord = EnglishWord ()
instance W.Word EnglishWord where 
    getWordTable = W.wordTable "/tmp/engwords"

instance Language English where 
    type POS (English) = P.POS 
    type NonTerm (English) = NT.NonTerm
    type Word (English) = W.WordWrap EnglishWord 
    isNP NT.NP = True
    isNP _ = False

    isNPB NT.NPB = True
    isNPB _ = False

    isVerb = P.isPOSVerb 
    isComma = P.isPOSComma
    isPunc = P.isPOSPunc
    
