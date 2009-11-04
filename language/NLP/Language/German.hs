{-# LANGUAGE TypeFamilies #-}
module NLP.Language.German where 

import NLP.Language
import qualified NLP.Language.German.NonTerm as NT
import qualified NLP.Language.German.POS as P
import qualified NLP.Language.Word as W
import qualified NLP.Language.English.POS as E

newtype German = German () 

newtype GermanWord = GermanWord ()
instance W.Word GermanWord where 
    getWordTable = W.wordTable "/tmp/gerwords"

instance Language German where 
    type POS (German) = P.POS 
    type NonTerm (German) = NT.NonTerm
    type Word (German) = W.WordWrap GermanWord 
    isNP NT.NP = True
    isNP _ = False

    isNPB NT.NPB = True
    isNPB _ = False

    isVerb = E.isPOSVerb . P.toJoint 
    isComma = E.isPOSComma . P.toJoint
    isPunc = E.isPOSPunc . P.toJoint
    
