{-# LANGUAGE TypeFamilies, FlexibleContexts  #-}
module NLP.Probability.Distribution (
  -- * Distributions
  --                    
  -- $DistDesc  
  Prob, Distribution, mle, laplace)  where 
import qualified Data.ListTrie.Base.Map as M
import Data.Maybe (fromMaybe)
import NLP.Probability.Observation

-- $DistDesc
-- Some very simple ways of estimating probabilities from observations. Will expand in the future.

type Prob = Double

type Distribution event = event -> Prob

type Estimator event = Observed event -> Distribution event

-- | Maximum Likelihood Estimation gives out probability by normalizing over observed events. 
--   Unseen events are gived zero probabilty. 
mle :: (Event event) => Estimator event
mle obs e = (fromMaybe 0.0 $ M.lookup e $ observed obs) / (total obs)

laplace :: (Event event) => (Double, Double) -> Estimator event
laplace (b, lambda) obs e = (count + lambda) / (n +  (b * lambda))
        where 
          count = fromMaybe 0.0 $ M.lookup e $ observed obs
          n = total obs

