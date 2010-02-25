module Probs where 

import Data.Binary 
import Adjunction


getCounts f = do
    counts <- decodeFile f 
    return (counts :: TAGCounts)

getProbs c = 
    estimateTAGProb c 

