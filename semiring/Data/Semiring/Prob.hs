{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Semiring.Prob where 
import Data.Semiring

-- | The 'Prob' semiring keeps track of the likelihood of the known output 
--   by keeping track of the probability of all paths. 
newtype Prob = Prob Double
    deriving (Eq, Show, Num, Real, Fractional, Ord) 

instance Multiplicative Prob where
    one = 1.0
    times = (*) 

instance Monoid Prob where 
    mempty = 0.0
    mappend = (+)

instance Semiring Prob 
instance WeightedSemiring Prob 
