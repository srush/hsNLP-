{-# LANGUAGE TypeFamilies, FlexibleContexts  #-}
module NLP.Probability.Distribution where 

import qualified Data.Map as M 
import Control.Monad (foldM, liftM)
import Data.Maybe (catMaybes)
import Data.Monoid
import NLP.Probability.Observation

type Prob = Double


data Distribution event = Distribution {
      prob :: event -> Prob
}

uniform n = Distribution $ \e -> 1/n

 
type Estimator event observed = observed -> Distribution event  

estimateMLE :: (Ord event) => Estimator event (Observed event)
estimateMLE obs = Distribution $ prob
        where 
          prob e = M.findWithDefault 0.0 e dist  
          total = calcTotal obs
          dist = M.map (/ total) $ counts obs

estimateLaplace :: (Ord event) => (Double, Double)-> Estimator event (Observed event)
estimateLaplace (b, lambda) obs =  Distribution prob
        where 
              n = calcTotal obs
              prob e =  (count + lambda) / (n +  (b * lambda))   
                  where count = M.findWithDefault 0.0 e $ counts obs


probMixed dists e = sum $ map (\(l, dist) -> l * prob dist e) dists

estimateMix :: [(Double, Estimator event observed)] -> Estimator event [observed] 
estimateMix ests obsList = Distribution $ probMixed dists
    where dists = map (\((l, est), obs) -> (l, est obs)) $ zip ests obsList 

type GeneralLambda observed = observed -> Double


estimateGeneralLinear :: [(GeneralLambda observed, 
                           Estimator event observed)] -> 
                         Estimator event [(observed)]
estimateGeneralLinear ests obsList = Distribution $ probMixed dists
    where dists = map (\((l, est), obs) -> (l obs, est obs)) $ zip ests obsList 

estimateLinearInterpolation :: (Ord event) => 
                               (Double,Double,Double) -> 
                               Estimator event [Observed event]
estimateLinearInterpolation (l1,l2,l3) = 
    estimateMix [( l1, estimateMLE),
                 ( l2, estimateMLE),
                 ( l3, estimateMLE)]

estimateWittenBell :: (Ord event) => Estimator event [Observed event]
estimateWittenBell = 
    estimateGeneralLinear [(const 0, estimateMLE), 
                           (lambdaComp, estimateMLE),
                           (lambda, estimateMLE)]
    where lambda obs = 1.0 - lambdaComp obs
          lambdaComp :: (Ord a) => Observed a -> Double
          lambdaComp obs = nonTrivial / (nonTrivial + total)
              where total = calcTotal obs 
                    nonTrivial = countNonTrivial obs

