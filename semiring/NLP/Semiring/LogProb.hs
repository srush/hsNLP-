{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module NLP.Semiring.LogProb where 
import NLP.Semiring


-- log prob, should only be used in viterbi semiring (add not defined)
newtype LogProb = LogProb Double
    deriving (Eq, Num, Real, Fractional, Floating, Ord) 

convertToProb (LogProb p) = exp(p)

convertToDouble (LogProb p) = p

instance Show LogProb where 
    show (LogProb p) = show p

instance Multiplicative LogProb where
    one = 0.0
    times = (+) 

instance Monoid LogProb where 
    mempty = undefined
    mappend = undefined

instance Semiring LogProb 
instance WeightedSemiring LogProb 
