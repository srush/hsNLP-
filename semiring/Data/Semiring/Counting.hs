{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Semiring.Counting where
import Data.Semiring

-- | The 'Counting' semiring keeps track of the number of paths 
--   or derivations led to a given output.
newtype Counting = Counting Integer
    deriving (Eq, Show, Num, Ord, Enum, Real, Integral) 

instance Multiplicative Counting where
    one = 1
    times = (*) 

instance Monoid Counting where 
    mempty = 0
    mappend = (+)


instance Semiring Counting 
instance WeightedSemiring Counting 

