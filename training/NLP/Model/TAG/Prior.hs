{-# LANGUAGE TypeSynonymInstances, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, FlexibleInstances #-}
module NLP.Model.TAG.Prior where 

--{{{  Imports
import NLP.Probability.Distribution
import NLP.Probability.Observation
import NLP.Probability.ConditionalDistribution
import qualified Data.Map as M
import System.IO.Unsafe
import Data.IORef
import NLP.Grammar.Spine
import NLP.Grammar.TAG
import NLP.Model.TAG.Wrap
import NLP.Language.SimpleLanguage
import NLP.Probability.Chain
import Helpers.Common
import Helpers.Test hiding (Counts)
--}}}



-- | This file gives a simple prior for beam pruning on a tree 


-- | A GWord context is smoo

type SubGWord = (Maybe AWord, Maybe APOS)
  
instance Context GWord where 
    type Sub GWord  = (SubGWord)
    type SubMap GWord = M.Map
    decompose gword = [(Nothing, Just $ getPOS gword),
                       (Just $ getLex gword, Nothing)]
     
instance Event GWord  where type EventMap (GWord) = M.Map
instance Event ASpine where type EventMap (ASpine) = M.Map


-- | The Prior is made up of the unigram probability of a GWord
--   And the conditional probability of a Spine given a GWord
    
data CollinsPrior = CollinsPrior
data PrPair = PrPair (GWord, ASpine)
            deriving (Eq, Ord, Show)

instance ChainedDist (PrPair) where 
    newtype Probs (PrPair) = 
        PriorProbs  (Distribution (GWord),
                     CondDistribution (ASpine) (GWord))

        
    newtype Observations (PrPair) =
        PriorObs (Counts GWord,
                  CondObserved ASpine GWord)
        deriving (Monoid, Binary)

    observe (PrPair (word, spine)) = PriorObs (observation $ word,
                                                   condObservation spine word)
                      
    prob (PriorProbs (udist, cdist)) = subProb
        where 
          subProb (PrPair (word, spine)) = p * (cdist  word spine)
              where p' = udist $ word
                    p = if isNaN p' then (1e-19) else max p' (1e-19)

estimatePrior :: Observations PrPair -> Probs PrPair   
estimatePrior (PriorObs (ucounts, ccounts) ) = 
    PriorProbs (mle $ finish ucounts, 
                mkDist $ estimateGeneralLinear (simpleLinear [0.7, 0.2, 0.1]) ccounts) 
    

instance JointModel CollinsPrior where 
    newtype FullEvent CollinsPrior  = PrEv (TWord) 
    newtype FullContext CollinsPrior = PrCon ()

    type Chained (CollinsPrior) = PrPair
        
    chainRule (PrEv tagword) _ = (PrPair (twWord tagword, twAtomSpine tagword))
--           subProb' tagword = unsafePerformIO $ do
--                        cacheMap <- readIORef cache
--                        case M.lookup tagword cacheMap of
--                          Just a -> return a 
--                          Nothing -> do
--                               let p = subProb tagword
--                               writeIORef cache $ M.insert tagword p cacheMap
--                               return p 
--           cache = unsafePerformIO $ newIORef M.empty


    


countPrior tagwords = 
    mconcat $ 
    do
      tword  <- tagwords
      return $ simpleObserve (PrEv tword) $ PrCon ()



--{{{  TESTS

testPrior = testGroup "Prior props" [

        ]





--}}}
