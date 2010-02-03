{-# LANGUAGE FlexibleInstances #-}
module NLP.Model.TAG.Wrap where 

import NLP.Grammar.TAG
import NLP.Grammar.Spine 
import NLP.Language.SimpleLanguage
import NLP.WordLattice
import NLP.ParseMonad

type TData = (GWord, ASpine)
type TWord = TAGWord  (ANonTerm) TData
type TSentence = TAGSentence (ANonTerm) TData

type RSpine = Spine NonTerm
type TSpine = Spine (ANonTerm) 
type ASpine = Atom (Spine (NonTerm))

twWord = fst . twData 
twAtomSpine = snd . twData 

instance WordSymbol (TAGWord ANonTerm TData) where 
    getLex = getLex . twWord
    getPOS = getPOS . twWord


twIsVerb :: ParseMonad (TWord ->  Bool)
twIsVerb = do
  fn <- isVerb
  return $ fn . getPOS . twWord

twIsComma :: ParseMonad (TWord ->  Bool) 
twIsComma = do
  fn <- isComma
  return $ fn . getPOS . twWord

--twIsConj _ = False

tSentence tsent = 
    sentFromArray sent 
        where (TAGSentence sent _) = tsent 