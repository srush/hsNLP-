{-# LANGUAGE TemplateHaskell, StandaloneDeriving, TypeFamilies, UndecidableInstances, Rank2Types, GeneralizedNewtypeDeriving, FlexibleInstances, TypeSynonymInstances, BangPatterns #-}
module NLP.Model.TAG.Adjunction where 

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
import NLP.Language.SimpleLanguage
import NLP.Grammar.TAG hiding (adjPos, adjType)
import NLP.Grammar.Spine
import Text.Printf
import NLP.Probability.Chain
import NLP.Model.TAG.Wrap
import Control.DeepSeq
import NLP.Grammar.Dependency
--}}}

emptyAdjunction = AdjunctionFullEvent Nothing Nothing Nothing Nothing Nothing Nothing Nothing


n = Nothing 
ji = Just . runIdentity
i = Identity

-- Adjunction Context1

type AdjunctionContext1 m =   
    (M7 m (ANonTerm) (Maybe ANonTerm) AdjunctionSide VerbDistance Delta (APOS) (AWord) )

--{{{  AdjunctionContext1 Classes
newtype AC1 = AC1 (AdjunctionContext1 Identity)
    deriving (Eq, Ord, Show, Binary)

type AdjunctionSubContext1 = AdjunctionContext1 Maybe

mkAdjCon1 ::(forall a. a -> m a) -> FullContext (Collins) -> AdjunctionContext1 m 
mkAdjCon1 fi (AdjunctionFullContext a b c d e f g _ _ _) = 
    M7 (fi a,fi b,fi c,fi d, fi e, fi f, fi g) 
       
instance Context (AC1 ) where 
    type SubMap (AC1) = M.Map
    type Sub AC1  = AdjunctionSubContext1
    decompose (AC1 (M7 (a,b,c,d,e,f,g))) = map M7 
        [(ji a,ji b,ji c,ji d,ji e,n,n),
         (n,n,n,n,n,ji f,n),
         (n,n,n,n,n,n,ji g)]
--}}}

-- Adjunction Event1 
type AdjunctionEvent1 = (Maybe (APOS), -- Child POS
                         Maybe (Maybe ANonTerm), -- Top NT
                         Maybe AdjunctionType -- is it sister
                        )

newtype AE1 = AE1 (AdjunctionEvent1)
    deriving (Eq, Ord, Show, Binary)

--{{{  AdjunctionEvent1 Classes


mkEvent1 event = AE1 (childPOS event, 
                      fmap (top) $ childSpine event,
                      adjType event) 
                 


instance Event (AE1) where type EventMap (AE1) = M.Map


instance Pretty(AE1) where 
    pPrint (AE1 (p, topNT, at)) = case p of 
                             Nothing -> text "END"
                             Just _ -> csep [hPretty p, hPretty topNT, hPretty at] 

--}}}

-- AdjunctionContext 2

type AdjunctionContext2 m = M3 m (Maybe (APOS)) (AE1 ) (AdjunctionContext1 m )

--{{{  AdjunctionContext2 Classes 
mkAdjCon2 ::(forall a. a -> m a) -> FullContext (Collins) -> AE1 -> AdjunctionContext2 m
mkAdjCon2 fi fullContext event1 = M3 (fi pos, fi $ event1, fi $ mkAdjCon1 fi fullContext) 
    where AE1 (pos, _, _) = event1 

newtype AC2 = AC2 (AdjunctionContext2 Identity)
    deriving (Eq, Ord, Show, Binary)

type AdjunctionSubContext2 = AdjunctionContext2 Maybe


instance Context (AC2) where 
    type SubMap (AC2 ) = M.Map
    type Sub (AC2)  = AdjunctionSubContext2
    decompose (AC2 (M3 (a,b,c))) = map M3 
        [(ji a,n, n),
         (n, ji b, Just d1),
         (n, n, Just d3)]
            where [d1, d2, d3] = decompose $ AC1 $ runIdentity c 
--}}}

newtype AE2 = AE2 (Maybe (AWord))
    deriving (Eq, Ord, Show, Binary)

--{{{  AdjunctionEvent2 Classes

mkEvent2 event = AE2 (childWord event) 

instance Event (AE2) where type EventMap (AE2) = M.Map

instance Pretty (AE2) where 
    pPrint (AE2 a) = case a of
                       Just _ -> hPretty a
                       Nothing -> text "END"

--}}}

-- AdjunctionContext 3

type AdjunctionContext3 m = M3 m (AE1 ) (AE2) (AdjunctionContext1 m)

--{{{  AdjunctionContext3 Classes 

mkAdjCon3 :: (forall a. a -> m a) -> FullContext (Collins) -> AE1 -> AE2 -> AdjunctionContext3 m
mkAdjCon3 fi fullContext event1 event2 = M3 (fi $ event1, 
                                             fi event2, 
                                             fi $ mkAdjCon1 fi fullContext) 

newtype AC3 = AC3 (AdjunctionContext3 Identity)
    deriving (Eq, Ord, Show, Binary)

type AdjunctionSubContext3  = AdjunctionContext3 Maybe 


instance Context (AC3) where 
    type SubMap (AC3) = M.Map
    type Sub (AC3 )  = AdjunctionSubContext3
    decompose (AC3 (M3 (a,b,c))) = map M3 
        [(ji a,n, n),
         (n, ji b, n),
         (n, n, Just d1)]
            where [d1, _, _] = decompose $ AC1 $ runIdentity c 
--}}}

newtype AE3 = AE3 (Maybe (ASpine))
    deriving (Eq, Ord, Show, Binary)
--{{{  AdjunctionEvent3 Classes
mkEvent3 event = AE3 (atomChildSpine event) 

instance Event (AE3) where type EventMap (AE3) = M.Map

instance Pretty (AE3) where 
    pPrint (AE3 a) = hPretty a 

--}}}

data Collins = Collins

instance JointModel (Collins) where 
     data Pairs (Collins) =  
         Pairs {probInfo :: ((AE1, AC1),
                             (AE2, AC2),
                             (AE3, AC3)),
                decisionInfo :: Maybe (Int, Int),
                enumVal :: (Int,Int)
               }
         deriving (Eq, Ord, Show)

     newtype Observation (Collins) = 
         Observation (CondObserved (AE1) (AC1),
                      CondObserved (AE2) (AC2),
                      CondObserved (AE3) (AC3))
         deriving (Monoid, Show, Pretty, Binary)

     data Probs (Collins) = 
         Probs (CondDistribution (AE1) (AC1),
                CondDistribution (AE2) (AC2),
                CondDistribution (AE3) (AC3))


     data FullEvent (Collins)   = AdjunctionFullEvent {
      childWord  :: Maybe (AWord),
      childPOS   :: Maybe (APOS),
      childSpine :: Maybe (TSpine), 
      atomChildSpine :: Maybe (ASpine), 
      childInd   :: Maybe Int, -- Not included in prob
      adjType    :: Maybe AdjunctionType,
      childTWord :: Maybe TWord -- Not included
    }
 
     data FullContext (Collins) =  AdjunctionFullContext { 
      parentNT   :: ANonTerm, 
      headNT     :: Maybe ANonTerm, 
      adjSide    :: AdjunctionSide, 
      crossesVerb :: VerbDistance, 
      delta      :: Delta,
      parentPOS  :: APOS ,
      parentWord :: AWord,
      parentInd  :: Int, -- not included in prob
      spinePos   :: Int,  -- not included
      parentTWord :: TWord -- Not included
    } 
     
     
     chainRule fullEvent fullContext = 
         Pairs ((e1, AC1 $ mkAdjCon1 Identity fullContext),
                (e2, AC2 $ mkAdjCon2 Identity fullContext e1),
                (e3, AC3 $ mkAdjCon3 Identity fullContext e1 e2))
               (fmap (\cInd -> (cInd, parentInd fullContext)) $ childInd fullEvent)
               (combineEnum [(fromEnum $ parentNT fullContext, 60),
                             (fromEnum $ headNT fullContext, 60),
                             (fromEnum $ adjSide fullContext, 3),
                             (fromEnum $ crossesVerb fullContext, 3),
                             (fromEnum $ delta fullContext, 20),
                             (fromEnum $ parentPOS fullContext, 60),
                             (fromEnum $ parentWord fullContext, 30000)]
                             ,
                combineEnum
                             [(fromEnum $ childPOS fullEvent, 60),
                             (fromEnum $ childWord fullEvent, 30000),
                             (fromEnum $ atomChildSpine fullEvent, 400),
                             (fromEnum $ atomChildSpine fullEvent, 400),
                             (fromEnum $ adjType fullEvent, 3)])
                            
         where e1 = mkEvent1 fullEvent
               e2 = mkEvent2 fullEvent
               e3 = mkEvent3 fullEvent

                    
     observe (Pairs (a,b,c)_ _) = 
         Observation $ (((uncurry condObservation) a), ((uncurry condObservation) b), ((uncurry condObservation) c))

     prob (Probs (p1,p2,p3)) (Pairs (a,b,c) _ _) =           
         ((uncurry $ flip p1) a) * ((uncurry $ flip p2) b) * ((uncurry $ flip p3) c)

     estimate (Observation (!obs1,  !obs2,  !obs3)) = 
         Probs (est $! obs1, est $! obs2, est $! obs3) 
         where  
           est :: (Event a, Context b) => CondObserved a b -> CondDistribution a b 
           est = estimateGeneralLinear (wittenBell 5)


