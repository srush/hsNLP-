{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, ScopedTypeVariables, FlexibleContexts, UndecidableInstances #-}
module NLP.Probability.Observation (
  -- * Observation
  --                    
  -- $ObsDesc                                      
  Count,
  Counts,
  Event(..), 
  observation,
  observations,
  inc,
  Observed(..),
  finish


                                   ) where 
import Data.Monoid
import Data.List (intercalate)
import Control.Monad (liftM)
import Data.Binary
import Text.PrettyPrint.HughesPJClass
import qualified Data.ListTrie.Base.Map as M
import Control.DeepSeq
-- $ObsDesc
-- This module provides a simple way to collect observations ('counts'), particularly within a monoid.  
-- Use 'observation' for each observed event and 'mappend' for combining observations. Finally 'finish' before estimating probabilities. 

type Count = Double

-- | Observations over a set of events. The param event must be an instance of class Event
newtype Counts event = Counts {
      counts :: (EventMap event) event Count 
} 



-- | Trivial type family for events. Just use EventMap = M.Map for most cases. Allows clients to specify the type of map used, when efficiency is important.   
class (M.Map (EventMap event) event) => Event event where 
    type EventMap event :: * -> * -> *

pShow fn (Counts counts) = vcat $ map (\(e,count) -> (fn e) <+> equals <+> double count ) $ M.toList counts 

instance (Event event, Pretty event) => Pretty (Counts event) where 
    pPrint = pShow pPrint
        
instance (Event event, Show event) => (Show (Counts event)) where 
    show = render . pShow (text. show)       
    
instance (Event event) => Monoid (Counts event) where 
    mempty = Counts M.empty 
    mappend (Counts a) (Counts b) = Counts $ M.unionWith (+) a b 

instance (Event event, Binary event, Binary ((EventMap event) event Count)) => 
         Binary (Counts event) where
    put (Counts m) = put m
    get = Counts `liftM` get 

instance (Event event, NFData event) => NFData (Counts event) where 
    rnf = rnf . M.toList . counts 

-- | Observation of a single event  
observation :: (Event event) => event -> Counts event
observation event = observations event 1.0

observations event count = Counts (M.singleton event count)  

-- | Manually increment the count of an event 
inc :: (Event e) => Counts e -> e -> Count -> Counts e
inc obs e c = obs {counts = M.insertWith (+) e c $ counts obs} 

observedEvents :: (Event event) => Counts event -> [event]
observedEvents (Counts m) = map fst $ filter ((> 0) . snd) $ M.toList m  

elems :: (M.Map map event) => map event elem -> [elem] 
elems = map snd . M.toList

calcTotal :: (Event event) => Counts event -> Count
calcTotal = sum . elems .counts 

countNonTrivial :: (Event event ) => Counts event -> Count
countNonTrivial = fromIntegral .length . filter (>0) . elems . counts 

data Observed event = Observed {
      observed :: (EventMap event) event Count,
      total  :: Double -- ^ Gives the total number of observations sum_a C(a)
      , unique :: Count -- ^ Gives the total number of events observed at least once {a | C(a) > 1}
} 

-- | Finish a set of offline observations so that they can be used to estimate
--   likelihood  
finish :: (Event event) => Counts event -> Observed event 
finish obs = Observed (counts obs) (calcTotal obs) (countNonTrivial obs)
