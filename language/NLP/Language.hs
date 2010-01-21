{-# LANGUAGE TypeFamilies, FlexibleContexts, ScopedTypeVariables, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
module NLP.Language where 
import Helpers.Common
import Helpers.MkEnum

    
class (Binary (POS a), Read (POS a), Show (POS a), Enum (POS a), Eq (POS a), Ord (POS a), Arbitrary (POS a), Bounded (POS a), Pretty (POS a), 
       Binary (NonTerm a), Show (NonTerm a), Enum (NonTerm a), Read (NonTerm a), Eq (NonTerm a), Ord (NonTerm a), Arbitrary (NonTerm a), Bounded (NonTerm a), Pretty (NonTerm a) , 
       Binary (Word a), Show (Word a), Enum (Word a), Eq (Word a), Ord (Word a), Arbitrary (Word a), Bounded (Word a), Pretty (Word a)
       )  
    => Language a where 
    data POS a
    data NonTerm a 
    data Word a 
    mkPOS :: String -> POS a
    mkNonTerm :: String -> NonTerm a 
    mkWord :: String -> Word a
    isNP :: NonTerm a -> Bool
    isNPB :: NonTerm a -> Bool

    isVerb :: POS a -> Bool
    isComma :: POS a -> Bool
    isPunc :: POS a -> Bool
    
-- | We often work with Word/POS combinations 
newtype GWord a = GWord (Word a, POS a)

getLex (GWord (l,_)) = l  
getPOS (GWord (_,p)) = p 

--{{{  GWord Classes
deriving instance (Language l) => Show (GWord l) 
deriving instance (Language l) => Eq (GWord l)
deriving instance (Language l) => Ord (GWord l) 
deriving instance (Language l) => Binary (GWord l) 

instance (Language l) => Enum (GWord l) where 
    fromEnum (GWord (a,b)) = mkFromEnum2 (a, maxBound) (b, maxBound)
    toEnum = GWord . mkToEnum2 (maxBound, maxBound)

instance (Language l) => Bounded (GWord l) where 
    minBound =  toEnum 0 
    maxBound = toEnum $ computeMaxBound [fromEnum $ (maxBound :: (Word l)),
                                         fromEnum $ (maxBound :: (POS l)) ]
--}}}
