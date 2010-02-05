{-# LANGUAGE TypeFamilies, TypeSynonymInstances #-}
module NLP.Model.Prior where

--{{{  Imports
import NLP.Probability.Distribution
import NLP.Probability.Observation
import NLP.Probability.ConditionalDistribution
import qualified Data.Map as M
import System.IO.Unsafe
import Data.IORef
import NLP.Grammar.Spine
import NLP.Grammar.TAG
import NLP.Model.TAGWrap
import NLP.Language
--}}}

-- This file gives a simple prior for a tree. 

-- | The Prior is made up of the unigram probability of a GWord
--   And the conditional probability of a Spine given a GWord

type PriorObs l = (Counts (GWord l),
                   CondObserved (TSpine l) (GWord l))

type PriorProbs l = (Distribution (GWord l),
                   CondDistribution (TSpine l) (GWord l))


-- | We smooth this by breaking a GWord down into its POS and its Word 

type SubGWord l = (Maybe (Word l), Maybe (POS l))
  
instance (Language l) => Context (GWord l) where 
    type Sub (GWord l) = (SubGWord l)
    type SubMap (GWord l) = M.Map
    decompose gword = [(Nothing, Just $ getPOS gword),
                       (Just $ getLex gword, Nothing)]
     
instance (Language l) => Event (GWord l) where type EventMap (GWord l) = M.Map
instance (Language l) => Event (TSpine l) where type EventMap (TSpine l) = M.Map

   

-- | Count collector
countSpine :: (Language l) => TWord l -> PriorObs l
countSpine tagword = 
    (observation $ twWord tagword,
     condObservation (twSpine tagword) $ twWord tagword)

probSpine :: (Language l) => (PriorProbs l) -> TWord l -> Prob
probSpine (udist, cdist) =
    subProb'
        where 
          subProb tagword = p * (cdist  (twWord tagword) $ twSpine tagword)
              where p' = (udist $ twWord tagword)
                    p = if isNaN p' then (1e-19) else max p' (1e-19)

          subProb' tagword = unsafePerformIO $ do
                       cacheMap <- readIORef cache
                       case M.lookup tagword cacheMap of
                         Just a -> return a 
                         Nothing -> do
                              let p = subProb tagword
                              writeIORef cache $ M.insert tagword p cacheMap
                              return p 
          cache = unsafePerformIO $ newIORef M.empty


-- | Turn observations into a distribution
estimatePrior :: (Language l) => PriorObs l -> PriorProbs l
estimatePrior (ucounts, ccounts) = (mle $ finish ucounts, 
                                    estimateGeneralLinear (simpleLinear [0.7, 0.2, 0.1]) ccounts) 
    