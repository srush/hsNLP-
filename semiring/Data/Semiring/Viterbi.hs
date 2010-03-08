
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Semiring.Viterbi where
import Data.Semiring
import Data.Semiring.ViterbiNBest

data One = One  
instance N One where 
    mkN = One
    n _ = 1

type Viterbi semi = ViterbiNBest One semi

mkViterbi v = ViterbiNBest [v]

fromViterbi :: (Semiring semi) => Viterbi semi -> semi 
fromViterbi (ViterbiNBest []) = mempty 
fromViterbi (ViterbiNBest [v]) =  v