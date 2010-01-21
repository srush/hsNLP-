{-# LANGUAGE TypeSynonymInstances, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module NLP.Model.ChainPrior where 

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
import NLP.Model.Chain
import Helpers.Common
--}}}



-- | This file gives a simple prior for beam pruning on a tree 


-- | A GWord context is smoo

type SubGWord l = (Maybe (Word l), Maybe (POS l))
  
instance (Language l) => Context (GWord l) where 
    type Sub (GWord l) = (SubGWord l)
    type SubMap (GWord l) = M.Map
    decompose gword = [(Nothing, Just $ getPOS gword),
                       (Just $ getLex gword, Nothing)]
     
instance (Language l) => Event (GWord l) where type EventMap (GWord l) = M.Map
instance (Language l) => Event (TSpine l) where type EventMap (TSpine l) = M.Map


-- | The Prior is made up of the unigram probability of a GWord
--   And the conditional probability of a Spine given a GWord
    
data CollinsPrior l = CollinsPrior

instance (Language l) => JointModel (CollinsPrior l) where 
    newtype FullEvent (CollinsPrior l) = PrEv (TWord l) 
    newtype FullContext (CollinsPrior l) = PrCon ()

    newtype Probs (CollinsPrior l) = 
        PriorProbs  (Distribution (GWord l),
                     CondDistribution (TSpine l) (GWord l))
 
    newtype Observation (CollinsPrior l) =
        PriorObs (Counts (GWord l),
                  CondObserved (TSpine l) (GWord l))
        deriving (Monoid, Binary)

    newtype Pairs (CollinsPrior l) = PrPair (TWord l)

    chainRule (PrEv tagword) _ = (PrPair tagword)
        
    observe (PrPair tagword) = PriorObs (observation $ twWord tagword,
                                condObservation (twSpine tagword) $ twWord tagword)
                      
    prob (PriorProbs (udist, cdist)) = subProb
        where 
          subProb (PrPair tagword) = p * (cdist  (twWord tagword) $ twSpine tagword)
              where p' = (udist $ twWord tagword)
                    p = if isNaN p' then (1e-19) else max p' (1e-19)

--           subProb' tagword = unsafePerformIO $ do
--                        cacheMap <- readIORef cache
--                        case M.lookup tagword cacheMap of
--                          Just a -> return a 
--                          Nothing -> do
--                               let p = subProb tagword
--                               writeIORef cache $ M.insert tagword p cacheMap
--                               return p 
--           cache = unsafePerformIO $ newIORef M.empty


    estimate (PriorObs (ucounts, ccounts) ) = 
        PriorProbs (mle $ finish ucounts, 
                    estimateGeneralLinear (simpleLinear [0.7, 0.2, 0.1]) ccounts) 
    