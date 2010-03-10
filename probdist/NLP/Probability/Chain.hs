{-# LANGUAGE TypeSynonymInstances, TypeSynonymInstances, TypeFamilies, FlexibleInstances, GeneralizedNewtypeDeriving, UndecidableInstances, TemplateHaskell, MultiParamTypeClasses, BangPatterns, StandaloneDeriving, FlexibleContexts #-}
module NLP.Probability.Chain  where 
import NLP.Probability.ConditionalDistribution
import NLP.Probability.Distribution 
import NLP.Probability.Observation
import qualified Data.Map as M
import Data.Monoid
import Data.Binary
import Text.PrettyPrint.HughesPJClass

type ChainedProbs a = Probs (Chained a) 
type ChainedObs a = Observations (Chained a)

class (ChainedDist (Chained a)) => JointModel a where 
    data FullEvent a
    data FullContext a
    type Chained a 
    chainRule :: FullEvent a -> FullContext a -> Chained a

class Estimate a where 
    type Dist a 

class ChainedDist a where 
    data Observations a
    data Probs a
    observe :: a -> Observations a
    prob :: Probs a -> a -> Prob
    --estimate :: Observations a -> Probs a

newtype P1 e c = P1 (e,c) 

instance (Event e, Context c) => ChainedDist (P1 e c) where 
    newtype Observations  (P1 e c) = Observation1 (CondObserved e c)

    newtype Probs (P1 e c) = Probs1 (CondDistribution e c)

    observe (P1 (e,c)) =  Observation1 $ condObservation e c 
    prob (Probs1 p1) (P1 (e, c)) = p1 c e

instance (Event a1, Event a2, 
          Context b1, Context b2) => ChainedDist (P1 a1 b1, P1 a2 b2) where 
    newtype Observations (P1 a1 b1, P1 a2 b2) = 
        Observation2 (CondObserved a1 b1,
                     CondObserved a2 b2)

    newtype Probs (P1 a1 b1, P1 a2 b2) = 
        Probs2 (CondDistribution a1 b1,
               CondDistribution a2 b2)
    
    observe (P1 p1, P1 p2) = Observation2 (uncurry condObservation p1, uncurry condObservation p2) 

    prob (Probs2 (p1, p2)) (P1 pa1, P1 pa2) = ((uncurry $ flip p1) pa1) * ((uncurry $ flip p2) pa2)


instance (Event a1, Event a2, Event a3, 
          Context b1, Context b2, Context b3) => ChainedDist (P1 a1 b1, P1 a2 b2, P1 a3 b3) where 
    newtype Observations (P1 a1 b1, P1 a2 b2, P1 a3 b3) = 
        Observation3 (CondObserved a1 b1,
                      CondObserved a2 b2,
                      CondObserved a3 b3)
        
    newtype Probs (P1 a1 b1, P1 a2 b2, P1 a3 b3) = 
        Probs3 (CondDistribution a1 b1,
                CondDistribution a2 b2,
                CondDistribution a3 b3)
    
    observe (P1 p1, P1 p2, P1 p3) = Observation3 (uncurry condObservation p1, 
                                         uncurry condObservation p2, 
                                         uncurry condObservation p3) 

    prob (Probs3 (p1, p2, p3)) (P1 pa1, P1 pa2, P1 pa3) = 
        ((uncurry $ flip p1) pa1) * 
        ((uncurry $ flip p2) pa2) * 
        ((uncurry $ flip p3) pa3)


instance Event String where type EventMap String = M.Map
instance Event Int where type EventMap Int = M.Map

simpleObserve a b = observe $ chainRule a b 