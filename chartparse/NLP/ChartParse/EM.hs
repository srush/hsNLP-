{-# LANGUAGE ScopedTypeVariables #-}

module NLP.Probability.EM where 


randomCounts :: (Bounded event, Enum event, Event event) => mr (Counts event)  
randomCounts = do
  mconcat $ [minBound..maxBound]

