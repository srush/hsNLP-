{-# LANGUAGE TemplateHaskell, StandaloneDeriving, TypeFamilies, UndecidableInstances, Rank2Types, GeneralizedNewtypeDeriving, FlexibleInstances, TypeSynonymInstances #-}
module NLP.Model.Adjunction where 

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
import NLP.Model.TAGWrap
--}}}

emptyAdjunction = AdjunctionFullEvent Nothing Nothing Nothing Nothing

n = Nothing 
ji = Just . runIdentity
i = Identity

-- Adjunction Context1

type AdjunctionContext1 m l =   
    (M7 m (NonTermWrap l) (NonTermWrap l) AdjunctionSide VerbDistance Delta (POS l) (Word l) )

--{{{  AdjunctionContext1 Classes
newtype AC1 l = AC1 (AdjunctionContext1 Identity l)

type AdjunctionSubContext1 l = AdjunctionContext1 Maybe l

mkAdjCon1 ::(forall a. a -> m a) -> FullContext (Collins l) -> AdjunctionContext1 m l
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
type AdjunctionEvent1 l = (Maybe (POS l), -- Child POS
                           Maybe (NonTermWrap l), -- Top NT
                           Maybe AdjunctionType -- is it sister
                          )
newtype AE1 l = AE1 (AdjunctionEvent1 l)
   

--{{{  AdjunctionEvent1 Classes


mkEvent1 event = AE1 (childPOS event, 
                      fmap (fromJustDef (fromPOS $ fromJustNote "childPOS" $ childPOS event) . top) $ childSpine event, 
                      adjType event) 

instance (Language l) => Event (AE1 l) where type EventMap (AE1 l) = M.Map
deriving instance (Language l) => Eq(AE1 l) 
deriving instance (Language l) => Ord(AE1 l)
deriving instance (Language l) => Show(AE1 l)
deriving instance (Language l) => Binary(AE1 l)

instance (Language l) => Pretty(AE1 l) where 
    pPrint (AE1 (p, topNT, at)) = case p of 
                             Nothing -> text "END"
                             Just _ -> csep [hPretty p, hPretty topNT, hPretty at] 

--}}}

-- AdjunctionContext 2

type AdjunctionContext2 m l = M3 m (Maybe (POS l)) (AE1 l) (AdjunctionContext1 m l)

--{{{  AdjunctionContext2 Classes 
mkAdjCon2 ::(forall a. a -> m a) -> FullContext (Collins l) -> AE1 l -> AdjunctionContext2 m l
mkAdjCon2 fi fullContext event1 = M3 (fi pos, fi $ event1, fi $ mkAdjCon1 fi fullContext) 
    where AE1 (pos, _, _) = event1 

newtype AC2 l = AC2 (AdjunctionContext2 Identity l)

type AdjunctionSubContext2 l = AdjunctionContext2 Maybe l

deriving instance (Language l) => Eq(AC2 l)
deriving instance (Language l) => Ord(AC2 l) 
deriving instance (Language l) => Show(AC2 l) 

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
deriving instance (Language l) => Show(AE2 l) 
deriving instance (Language l) => Eq(AE2 l)
deriving instance (Language l) => Ord(AE2 l) 
deriving instance (Language l) => Binary(AE2 l)

instance (Language l) => Pretty (AE2 l) where 
    pPrint (AE2 a) = case a of
                       Just _ -> hPretty a
                       Nothing -> text "END"

--}}}

-- AdjunctionContext 3

type AdjunctionContext3 m l = M3 m (AE1 l) (AE2 l) (AdjunctionContext1 m l)

--{{{  AdjunctionContext3 Classes 

mkAdjCon3 :: (forall a. a -> m a) -> FullContext (Collins l) -> AE1 l -> AE2 l -> AdjunctionContext3 m l
mkAdjCon3 fi fullContext event1 event2 = M3 (fi $ event1, 
                                             fi event2, 
                                             fi $ mkAdjCon1 fi fullContext) 

newtype AC3 l = AC3 (AdjunctionContext3 Identity l)

type AdjunctionSubContext3 l = AdjunctionContext3 Maybe l

deriving instance (Language l) => Eq(AC3 l)
deriving instance (Language l) => Ord(AC3 l) 

instance (Language l) => Context (AC3 l) where 
    type SubMap (AC3 l) = M.Map
    type Sub (AC3 l)  = AdjunctionSubContext3 l
    decompose (AC3 (M3 (a,b,c))) = map M3 
        [(ji a,n, n),
         (n, ji b, n),
         (n, n, Just d1)]
            where [d1, _, _] = decompose $ AC1 $ runIdentity c 
--}}}

newtype AE3 l = AE3 (Maybe (TSpine l))

--{{{  AdjunctionEvent3 Classes
mkEvent3 event = AE3 (childSpine event) 

deriving instance (Language l) => Show(AE3 l) 
deriving instance (Language l) => Eq(AE3 l) 
deriving instance (Language l) => Ord(AE3 l) 
deriving instance (Language l) => Binary(AE3 l)

instance (Language l) => Event (AE3 l) where type EventMap (AE3 l) = M.Map

instance (Language l) => Pretty (AE3 l) where 
    pPrint (AE3 a) = hPretty a 

--}}}

data Collins l = Collins

instance (Language l) => JointModel (Collins l) where 
     data Pairs (Collins l) =  Pairs ((AE1 l, AC1 l),
                                      (AE2 l, AC2 l),
                                      (AE3 l, AC3 l))
     
     newtype Observation (Collins l) = Observation (CondObserved (AE1 l) (AC1 l),
                                     CondObserved (AE2 l) (AC2 l),
                                     CondObserved (AE3 l) (AC3 l))
         deriving (Monoid, Show, Pretty, Binary)

     data Probs (Collins l) = Probs (CondDistribution (AE1 l) (AC1 l),
                               CondDistribution (AE2 l) (AC2 l),
                               CondDistribution (AE3 l) (AC3 l))


     data FullEvent (Collins l)   = AdjunctionFullEvent {
      childWord :: Maybe (Word l),
      childPOS :: Maybe (POS l),
      childSpine :: Maybe (TSpine l), 
      adjType :: Maybe AdjunctionType
    }
 
     data FullContext (Collins l) =  AdjunctionFullContext { 
      parentNT :: NonTermWrap l, 
      headNT :: NonTermWrap l, 
      adjSide :: AdjunctionSide, 
      crossesVerb :: VerbDistance, 
      delta :: Delta,
      parentPOS :: POS l,
      parentWord :: Word l
    } 
     
     
     chainRule fullEvent fullContext = Pairs ((e1, AC1 $ mkAdjCon1 Identity fullContext),
                                        (e2, AC2 $ mkAdjCon2 Identity fullContext e1),
                                        (mkEvent3 fullEvent, AC3 $ mkAdjCon3 Identity fullContext e1 e2) )
         where e1 = mkEvent1 fullEvent
               e2 = mkEvent2 fullEvent

     observe (Pairs (a,b, c)) = Observation $ (((uncurry condObservation) a), ((uncurry condObservation) b), ((uncurry condObservation) c))

     prob (Probs (p1,p2,p3)) (Pairs (a,b,c)) =  ((uncurry $ flip p1) a) * ((uncurry $ flip p2) b) * ((uncurry $ flip p3) c)

     estimate (Observation ( obs1,  obs2,  obs3)) = Probs (est obs1, est obs2, est obs3) 
         where  
           est :: (Event a, Context b) => CondObserved a b -> CondDistribution a b 
           est = estimateGeneralLinear (wittenBell 5)


