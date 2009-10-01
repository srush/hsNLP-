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
type PriorObs = (Observed GWord,
                 CondObserved Spine GWord)

type PriorProbs = (Distribution GWord,
                   CondDistribution Spine GWord)

type SubGWord = (Maybe Word, Maybe POS)
  
instance Context GWord where 
    type Sub GWord = SubGWord
    decompose (word, pos) = [(Nothing, Just pos),
                             (Just word, Nothing)]
        
countSpine :: TAGWord -> PriorObs
countSpine tagword = 
    (singleton $ twWord tagword,
     singletonObservation (twSpine tagword) $ twWord tagword 
     )

probSpine (udist, cdist) tagword =
    (prob udist $ twWord tagword) * (prob (cond cdist $ twWord tagword) $ twSpine tagword)


estimatePrior :: PriorObs -> PriorProbs
estimatePrior (ucounts, ccounts) = (estimateMLE ucounts, 
                                    estimateLinear [0.7, 0.3] ccounts) 
    

