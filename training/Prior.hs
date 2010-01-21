{-# LANGUAGE TypeFamilies, TypeSynonymInstances #-}
module Prior where

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

type PriorObs l = (Counts (GWord l),
                   CondObserved (TSpine l))

type PriorProbs = (Distribution GWord,
                   CondDistribution Spine GWord)

type SubGWord = (Maybe Word, Maybe POS)
  
instance Context GWord where 
    type Sub GWord = SubGWord
    type SubMap GWord = M.Map
    decompose (word, pos) = [(Nothing, Just pos),
                             (Just word, Nothing)]
     
instance Event GWord where type EventMap GWord = M.Map
instance Event Spine where type EventMap Spine = M.Map
   
countSpine :: TAGWord -> PriorObs
countSpine tagword = 
    (observation $ twWord tagword,
     condObservation (twSpine tagword) $ twWord tagword 
     )

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

estimatePrior :: PriorObs -> PriorProbs
estimatePrior (ucounts, ccounts) = (mle $ finish ucounts, 
                                    estimateGeneralLinear (simpleLinear [0.7, 0.2, 0.1]) ccounts) 
    

