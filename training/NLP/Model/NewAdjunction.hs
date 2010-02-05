{-# LANGUAGE TypeFamilies, UndecidableInstances, Rank2Types#-}
module NewAdjunction where 

--{{{  Imports
import Helpers.Common 
import NLP.Probability.ConditionalDistribution
import NLP.Probability.Distribution 
import NLP.Probability.Observation
import Control.Monad.Identity
import Helpers.MkEnum
import NLP.Model.Distance
import qualified Data.Map as M
import qualified Data.ListTrie.Base.Map as LT
import qualified Data.Bimap as BM 
import NLP.Language
import NLP.Grammar.TAG hiding (adjPos, adjType)
import NLP.Grammar.Spine
import NLP.Grammar.NonTerm
import Text.Printf
import NLP.Model.Chain
--}}}

data AdjunctionFullContext l = AdjunctionFullContext { 
      parentNT :: NonTerm l, 
      headNT :: NonTerm l, 
      adjSide :: AdjunctionSide, 
      crossesVerb :: VerbDistance, 
      delta :: Delta,
      parentPOS :: POS l,
      parentWord :: Word l
    }

data AdjunctionFullEvent l = AdjunctionFullEvent {
      childWord :: Maybe (Word l),
      childPOS :: Maybe (POS l),
      childSpine :: Maybe (Spine l), 
      adjType :: Maybe AdjunctionType
}

n = Nothing 
ji = Just . runIdentity
i = Identity

-- Adjunction Context1

type AdjunctionContext1 m l =   
    (M7 m (NonTerm l) (NonTerm l) AdjunctionSide VerbDistance Delta (POS l) (Word l) )

--{{{  AdjunctionContext1 Classes
newtype AC1 l = AC1 (AdjunctionContext1 Identity l)

instance (Language l) => Eq(AC1 l)
instance (Language l) => Ord(AC1 l) 


type AdjunctionSubContext1 l = AdjunctionContext1 Maybe l

mkAdjCon1 ::(forall a. a -> m a) -> AdjunctionFullContext l -> AdjunctionContext1 m l
mkAdjCon1 fi (AdjunctionFullContext a b c d e f g) = 
    M7 (fi a,fi b,fi c,fi d, fi e, fi f, fi g) 
       
instance (Language l) => Context (AC1 l) where 
    type SubMap (AC1 l) = M.Map
    type Sub (AC1 l)  = AdjunctionSubContext1 l
    decompose (AC1 (M7 (a,b,c,d,e,f,g))) = map M7 
        [(ji a,ji b,ji c,ji d,ji e,n,n),
         (n,n,n,n,n,ji f,n),
         (n,n,n,n,n,n,ji g)]
--}}}

-- Adjunction Event1 
type AdjunctionEvent1 l = (Maybe (POS l), Maybe AdjunctionType)

--{{{  AdjunctionEvent1 Classes
newtype AE1 l = AE1 (AdjunctionEvent1 l)

mkEvent1 event = AE1 (childPOS event, adjType event) 

instance (Language l) => Event (AE1 l) where type EventMap (AE1 l) = M.Map
instance (Language l) => Eq(AE1 l)
instance (Language l) => Ord(AE1 l) 
--}}}

-- AdjunctionContext 2

type AdjunctionContext2 m l = M3 m (Maybe (POS l)) (AE1 l) (AdjunctionContext1 m l)

--{{{  AdjunctionContext2 Classes 
mkAdjCon2 ::(forall a. a -> m a) -> AdjunctionFullContext l -> AE1 l -> AdjunctionContext2 m l
mkAdjCon2 fi fullContext event1 = M3 (fi pos, fi $ event1, fi $ mkAdjCon1 fi fullContext) 
    where AE1 (pos, _) = event1 

newtype AC2 l = AC2 (AdjunctionContext2 Identity l)

type AdjunctionSubContext2 l = AdjunctionContext2 Maybe l

instance (Language l) => Eq(AC2 l)
instance (Language l) => Ord(AC2 l) 

instance (Language l) => Context (AC2 l) where 
    type SubMap (AC2 l) = M.Map
    type Sub (AC2 l)  = AdjunctionSubContext2 l
    decompose (AC2 (M3 (a,b,c))) = map M3 
        [(ji a,n, n),
         (n, ji b, Just d1),
         (n, n, Just d3)]
            where [d1, d2, d3] = decompose $ AC1 $ runIdentity c 
--}}}

newtype AE2 l = AE2 (Maybe (Word l))

--{{{  AdjunctionEvent2 Classes

mkEvent2 event = AE2 (childWord event) 

instance (Language l) => Event (AE2 l) where type EventMap (AE2 l) = M.Map
instance (Language l) => Eq(AE2 l)
instance (Language l) => Ord(AE2 l) 
--}}}

-- AdjunctionContext 3

type AdjunctionContext3 m l = M3 m (AE1 l) (AE2 l) (AdjunctionContext1 m l)

--{{{  AdjunctionContext2 Classes 

mkAdjCon3 :: (forall a. a -> m a) -> AdjunctionFullContext l -> AE1 l -> AE2 l -> AdjunctionContext3 m l
mkAdjCon3 fi fullContext event1 event2 = M3 (fi $ event1, fi event2, fi $ mkAdjCon1 fi fullContext) 

newtype AC3 l = AC3 (AdjunctionContext3 Identity l)

type AdjunctionSubContext3 l = AdjunctionContext3 Maybe l

instance (Language l) => Eq(AC3 l)
instance (Language l) => Ord(AC3 l) 

instance (Language l) => Context (AC3 l) where 
    type SubMap (AC3 l) = M.Map
    type Sub (AC3 l)  = AdjunctionSubContext3 l
    decompose (AC3 (M3 (a,b,c))) = map M3 
        [(ji a,n, n),
         (n, ji b, n),
         (n, n, Just d3)]
            where [_, _, d3] = decompose $ AC1 $ runIdentity c 
--}}}

newtype AE3 l = AE3 (Maybe (Spine l))

--{{{  AdjunctionEvent2 Classes
mkEvent3 event = AE3 (childSpine event) 

instance (Language l) => Event (AE3 l) where type EventMap (AE3 l) = M.Map
instance (Language l) => Eq(AE3 l)
instance (Language l) => Ord(AE3 l) 
--}}}

data Collins l = Collins ()
instance (Language l) => JointModel (Collins l) where 
     type FullEvent (Collins l)  = AdjunctionFullEvent l 

     type FullContext (Collins l) = AdjunctionFullContext l 
     
     type Pairs (Collins l) =  ((AE1 l, AC1 l),
                                (AE2 l, AC2 l),
                                (AE3 l, AC3 l))
     
     type Observation (Collins l) = (CondObserved (AE1 l) (AC1 l),
                                     CondObserved (AE2 l) (AC2 l),
                                     CondObserved (AE3 l) (AC3 l))
     
     type Probs (Collins l) = (CondDistribution (AE1 l) (AC1 l),
                               CondDistribution (AE2 l) (AC2 l),
                               CondDistribution (AE3 l) (AC3 l))
     
     chainRule fullEvent fullContext = ((e1, AC1 $ mkAdjCon1 Identity fullContext),
                                        (e2, AC2 $ mkAdjCon2 Identity fullContext e1),
                                        (mkEvent3 fullEvent, AC3 $ mkAdjCon3 Identity fullContext e1 e2) )
         where e1 = mkEvent1 fullEvent
               e2 = mkEvent2 fullEvent
     observe (a,b,c ) = (((uncurry condObservation) a), ((uncurry condObservation) b), ((uncurry condObservation) c))

     prob (p1,p2,p3) (a,b,c) = ((uncurry $ flip p1) a) * ((uncurry $ flip p2) b) * ((uncurry $ flip p3) c)

     estimate (obs1, obs2, obs3) = (est obs1, est obs2, est obs3) 
         where  
           est :: (Event a, Context b) => CondObserved a b -> CondDistribution a b 
           est = estimateGeneralLinear (wittenBell 5)