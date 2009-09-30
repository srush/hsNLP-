{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell, ScopedTypeVariables #-}
module NLP.Probability.Observation where 
import qualified Data.IntMap as M 
import Data.Monoid
import Text.Printf
import Data.List (intercalate)
import Data.Generics 
import Data.Binary
import Text.PrettyPrint.HughesPJClass

type Count = Double

newtype Observed event = Observed {
      counts :: M.IntMap Count 
} deriving (Eq, Ord, Binary)




instance (Show event) => (Show (Observed event)) where 
    show (Observed counts) = 
        "[ " ++ (intercalate ","  $ map (\(event, count) -> printf "%s: %s" (show event) (show count)) $ M.toList counts) ++ " ]"

instance (Pretty event, Enum event) => Pretty (Observed event) where 
    pPrint (Observed counts) = 
        vcat $ map (\(i,count) -> (pPrint $ (toEnum i::event)) <+> equals <+> double count  ) $ M.toList counts 
    

instance (Enum event) => Monoid (Observed event) where 
    mempty = Observed mempty 
    mappend (Observed a) (Observed b) = Observed $ M.unionWith (+) a b 

singleton :: (Enum event) => event -> Observed event
singleton event = Observed (M.singleton (fromEnum event) 1)  

observedEvents :: (Enum event) => Observed event -> [event]
observedEvents (Observed m) = map (toEnum.fst) $ filter ((> 0) . snd) $ M.toList m  

inc :: (Enum e) => Observed e -> e -> Count -> Observed e
inc obs e c = obs {counts = M.insertWith (+) (fromEnum e) c $ counts obs} 

inc1 :: (Enum e) => Observed e -> e -> Observed e 
inc1 obs e = inc obs e 1.0


data ExtraObserved event = ExtraObserved {
      eoObserved :: Observed event,
      eoTotal  :: Double, 
      eoUnique :: Count
} deriving Show


storeState :: (Enum event) => Observed event -> ExtraObserved event 
storeState obs = ExtraObserved obs (calcTotal obs) (countNonTrivial obs)

-- | Gives the total number of observations
--   sum_a C(a)
calcTotal :: (Enum e) => Observed e -> Count
calcTotal = sum . M.elems .counts 


-- | Gives the total number of events observed at least once
--   |{a | C(a) > 1}|
countNonTrivial :: (Enum e) => Observed e -> Count
countNonTrivial = fromIntegral .length . filter (>0) . M.elems .counts 

