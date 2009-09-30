{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Prior where

import NLP.Probability.Distribution
import NLP.Probability.Observation
import NLP.Probability.ConditionalDistribution
import TAG
import Sentence
import NonTerm

type PriorObs = (Observed GWord,
                 CondObserved Spine GWord)

type PriorProbs = (Distribution GWord,
                   CondDistribution Spine GWord) 
instance Context GWord where 
    type Sub GWord = GWord
    decompose g = [g]
        
countSpine :: TAGWord -> PriorObs
countSpine tagword = 
    (singleton $ twWord tagword,
     singletonObservation (twSpine tagword) $ twWord tagword 
     )

probSpine (udist, cdist) tagword =
    (prob udist $ twWord tagword) * (prob (cond cdist $ twWord tagword) $ twSpine tagword)


estimatePrior :: PriorObs -> PriorProbs
estimatePrior (ucounts, ccounts) = (estimateMLE ucounts, 
                                    estimateConditional (estimateMix [(1.0,estimateMLE)]) ccounts) 
    