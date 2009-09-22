{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module NLP.Probability.Observation where 
import qualified Data.Map as M 
import Data.Monoid
import Text.Printf
import Data.List (intercalate)
import Data.Generics 
import Data.Binary

type Count = Double

newtype Observed event = Observed {
      counts :: M.Map event Count 
} deriving (Eq, Ord, Binary)


instance (Show event) => (Show (Observed event)) where 
    show (Observed counts) = "[ " ++ (intercalate ","  $ map (\(event, count) -> printf "%s: %s" (show event) (show count)) $ M.toList counts) ++ " ]"


instance (Ord event) => Monoid (Observed event) where 
    mempty = Observed mempty 
    mappend (Observed a) (Observed b) = Observed $ M.unionWith (+) a b 

singleton event = Observed (M.singleton event 1)  

observedEvents (Observed m) = map fst $ filter ((> 0) . snd) $ M.toList m  

inc :: (Ord e) => Observed e -> e -> Count -> Observed e
inc obs e c = obs {counts = M.insertWith (+) e c $ counts obs} 

inc1 :: (Ord e) => Observed e -> e -> Observed e 
inc1 obs e = inc obs e 1.0

-- | Gives the total number of observations
--   sum_a C(a)
calcTotal :: (Ord e) => Observed e -> Count
calcTotal = sum . M.elems .counts 


-- | Gives the total number of events observed at least once
--   |{a | C(a) > 1}|
countNonTrivial :: (Ord e) => Observed e -> Count
countNonTrivial = fromIntegral .length . filter (>0) . M.elems .counts 
