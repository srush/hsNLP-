{-# LANGUAGE ScopedTypeVariables #-}

module NLP.Probability.EM where 
import NLP.Probability.Observation
import NLP.Probability.ConditionalDistribution
import Control.Monad.Random
import Control.Monad (liftM)
import Data.Monoid

randomCounts :: (Bounded event, Enum event, Event event, MonadRandom mr) => 
                mr (Counts event)  
randomCounts = do
  rcounts <- mapM (\e -> do {r <- getRandomR (1, 10); return (e,r)}) [minBound..maxBound]
  return $ mconcat $ map (uncurry observations) rcounts  

randomCondCounts :: (Bounded event, Enum event , Event event ,
                     Bounded context, Enum context, Context context,
                    MonadRandom mr) => [context] -> mr (CondObserved event context)
randomCondCounts contexts =  do
  let r = randomCounts
  condcounts <- mapM (\context -> condObservationCounts context `liftM` r) contexts
  return $ mconcat condcounts

