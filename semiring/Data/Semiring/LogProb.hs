{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Semiring.LogProb where 
import Data.Semiring


-- log prob, should only be used in viterbi semiring (add not defined)
newtype LogProb = LogProb Double
    deriving (Eq, Ord) 

convertToProb (LogProb p) = exp(p)

convertToDouble (LogProb p) = p

fromProb p = LogProb $ log p 

instance Show LogProb where 
    show (LogProb p) = show p

instance Multiplicative LogProb where
    one = LogProb 0.0
    times (LogProb a) (LogProb b) = LogProb (a + b)

instance Monoid LogProb where 
    mempty = undefined
    mappend = undefined

instance Semiring LogProb 
instance WeightedSemiring LogProb 
