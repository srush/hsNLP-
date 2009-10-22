{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, Rank2Types, FlexibleContexts #-}
module NLP.Probability.ConditionalDistribution (  
  -- * Conditional Distributions
  --                    
  -- $CondDistDesc  
                                                CondObserved(),
                                                CondDistribution,
                                                condObservation,
                                                Context(..), 
                                                estimateGeneralLinear,
                                                Weighting,
                                                wittenBell, 
                                                simpleLinear 
                                                ) where 
import qualified Data.ListTrie.Base.Map as M
import Data.List (inits)
import Data.Monoid
import qualified NLP.Probability.SmoothTrie as ST
import NLP.Probability.Distribution
import NLP.Probability.Observation 
import Data.Binary

-- $CondDistDesc
-- Say we want to estimate a conditional distribution based on a very large set of observed data.
-- Naively, we could just collect all the data and estimate a large table, but
-- our table would have little or no counts for a feasible future observations. 
--
-- In practice, we use smoothing to supplement rare contexts with data from similar, more often seen contexts. For instance,
-- using bigram probabilities when the given trigrams observations are too sparse. 
-- Most of these smoothing techniques are special cases of general linear interpolation, which chooses the weight of 
-- each level of smoothing based on the sparsity of the current context. 
--
-- In this module, we give an implementation of this process that separates out count collection
-- from the smoothing model, using  a Trie. The user specifies a Context instance that relates the full conditional context
-- to a sequences of SubContexts that characterize the levels of smoothing and the transitions in the Trie. We also give a small set of smoothing techniques 
-- to combine these levels. 
--
-- This work is based on Chapter 6 of ''Foundations of Statistical Natural Language Processing'' 
-- by Chris Manning and Hinrich Schutze. 
-- 


-- | The set of observations of event conditioned on context. event must be an instance of Event and context of Context 
type CondObserved event context = (ST.SmoothTrie (SubMap context) (Sub context) (Counts event))

-- | Events are conditioned on Contexts. When Contexts are sparse, we need a way to decompose into simpler SubContexts. 
--   This class allows us to separate this decomposition from the collection of larger contexts. 
class (M.Map (SubMap a) (Sub a)) => Context a where 
    -- | The type of sub contexts
    type Sub a  
    -- | A map over subcontexts (for efficiency) 
    type SubMap a :: * -> * -> * 
    -- | A function to enumerate subcontexts of a context  
    decompose ::  a -> [Sub a] 

-- | A CondObserved set for a single event and context. 
condObservation :: (Context context, Event event) => 
             event -> context -> CondObserved event context
condObservation event context = 
    ST.addColumn decomp observed mempty 
        where observed = observation event 
              decomp = decompose context 

type CondDistribution event context = context -> Distribution event


type Weighting = forall a. [Maybe (Observed a)] -> [Double]


-- | General Linear Interpolation. Produces a Conditional Distribution from observations.
--   It requires a GeneralLambda function which tells it how to weight each level of smoothing. 
--   The GeneralLambda function can observe the counts of each level of context. 
--
--   Note: We include a final level of backoff where everything is given an epsilon likelihood. To 
--   ignore this, just give it lambda = 0.
estimateGeneralLinear :: (Event event, Context context) => 
                         Weighting -> 
                         CondObserved event context -> 
                         CondDistribution event context
estimateGeneralLinear genLambda cstat = conFun 
    where
      conFun context = (\event -> sum $ zipWith (*) lambdas $ map (probE event) stats) 
          where stats = reverse $ 
                        Nothing : (map (\k -> Just $ ST.lookupWithDefault (finish mempty) k cstat')  $ 
                                  tail $ inits $ decompose context)
                probE event (Just dist) = if isNaN p then 0.0 else p
                    where p = mle dist event
                probE event Nothing = 1e-19
                lambdas = genLambda stats                
      cstat' = fmap finish cstat

-- | Weight each level by a fixed predefined amount. 
simpleLinear :: [Double] -> Weighting
simpleLinear lambdas = const lambdas


lambdaWBC :: Int -> Observed b -> Double
lambdaWBC n eobs = total' / (((fromIntegral n) * distinct) + total')
    where total' = total eobs
          distinct = unique eobs

-- | Weight each level by the likelihood that a new event will be seen at that level. 
--   t / ((n * d) + t) where t is the total count, d is the number of distinct observations,
--   and n is a user defined constant.   
wittenBell :: Int -> Weighting 
wittenBell n ls = wittenBell' ls 1.0
    where 
      wittenBell' [Nothing] mult = [mult]
      wittenBell' (Just cur:ls) mult = 
          if total cur > 0 then (l*mult : wittenBell' ls ((1-l)*mult)) 
          else (0.0: wittenBell' ls mult)  
              where l = lambdaWBC n cur

