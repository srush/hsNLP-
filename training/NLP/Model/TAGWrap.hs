module NLP.Model.TAGWrap where 
import NLP.Grammar.NonTerm
import NLP.Grammar.TAG
import NLP.Grammar.Spine 
import NLP.Language
import NLP.WordLattice
type TWord l = TAGWord  (NonTermWrap l) (GWord l)
type TSentence l = TAGSentence (NonTermWrap l) (GWord l)
type TSpine l = Spine (NonTermWrap l) 


twIsVerb (TAGWord _ (GWord (_,pos)) _ ) = isVerb pos 
twIsComma (TAGWord _ (GWord (_,pos)) _) = isComma pos 
twIsConj _ = False

tSentence tsent = 
    sentFromArray sent 
        where (TAGSentence sent _) = tsent 