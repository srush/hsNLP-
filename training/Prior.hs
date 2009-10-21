{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Prior where

import NLP.Probability.Distribution
import NLP.Probability.Observation
import NLP.Probability.ConditionalDistribution
import TAG
import Sentence
import NonTerm
import Word 
import POS
import qualified Data.Map as M
import System.IO.Unsafe
import Data.IORef

type PriorObs = (Observed GWord,
                 CondObserved Spine GWord)

type PriorProbs = (Distribution GWord,
                   CondDistribution Spine GWord)

type SubGWord = (Maybe Word, Maybe POS)
  
instance Context GWord where 
    type Sub GWord = SubGWord
    type SubMap GWord = M.Map
    decompose (word, pos) = [(Nothing, Just pos),
                             (Just word, Nothing)]
        
countSpine :: TAGWord -> PriorObs
countSpine tagword = 
    (singleton $ twWord tagword,
     singletonObservation (twSpine tagword) $ twWord tagword 
     )

probSpine (udist, cdist) =
    subProb'

        where 
          subProb tagword = p * (prob (cond cdist $ twWord tagword) $ twSpine tagword)
              where p' = (prob udist $ twWord tagword)
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
estimatePrior (ucounts, ccounts) = (estimateMLE ucounts, 
                                    estimateLinear [0.7, 0.3] ccounts) 
    

