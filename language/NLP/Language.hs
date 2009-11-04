{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module NLP.Language where 
    
class (Show (POS a), Enum (POS a), Show (Word a), Show (NonTerm a), Enum (NonTerm a),
       Enum (Word a))  
    => Language a where 
    type POS a
    type NonTerm a 
    type Word a 
    isNP :: NonTerm a -> Bool
    isNPB :: NonTerm a -> Bool

    isVerb :: POS a -> Bool
    isComma :: POS a -> Bool
    isPunc :: POS a -> Bool
    
