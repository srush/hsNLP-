{-# LANGUAGE TemplateHaskell, StandaloneDeriving, TypeFamilies, UndecidableInstances, Rank2Types, GeneralizedNewtypeDeriving, FlexibleInstances, TypeSynonymInstances, BangPatterns #-}
module NLP.Model.Dependency.Model where 

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
import Text.Printf
import NLP.Probability.Chain
import Control.DeepSeq
import NLP.Grammar.Dependency
--}}}

n = Nothing 
ji = Just . runIdentity
i = Identity


type DependencyEvent1 = (APOS, ALabel) 
newtype DE1 = DE1 (DependencyEvent1)
    deriving (Eq, Ord, Show, Binary)
instance Event (DE1) where type EventMap (DE1) = M.Map

mkEvent1 fe = DE1 (childPOS fe, childLabel fe)
                         
type DependencyContext1 m = 
    M5 m Delta VerbDistance AdjunctionSide APOS AWord

newtype DC1 = DC1 (DependencyContext1 Identity)
    deriving (Eq, Ord, Show, Binary)

mkAdjCon1 :: (forall a. a-> m a) -> FullContext FirstOrderDep -> DependencyContext1 m
mkAdjCon1 fi fc = M5 ((fi $ delta fc), (fi $ crossesVerb fc), (fi$ adjSide fc), (fi $ parentPOS fc), (fi $ parentWord fc))
type DependencySubContext1 = DependencyContext1 Maybe
instance Context DC1 where 
    type SubMap DC1 = M.Map
    type Sub DC1  = DependencySubContext1
    decompose (DC1 (M5 (a,b,c,d,e))) = map M5 
        [(ji a,ji b,ji c,n,n),
         (n,n,n,ji d,n),
         (n,n,n,n,ji e)]


type DependencyEvent2 = AWord
newtype DE2 = DE2 DependencyEvent2
    deriving (Eq, Ord, Show, Binary)
instance Event (DE2) where type EventMap DE2 = M.Map

mkEvent2 fe = DE2 (childWord fe)

type DependencyContext2 m = M3 m APOS ALabel (DependencyContext1 m)

mkAdjCon2 :: (forall a. a-> m a) -> FullContext FirstOrderDep -> DE1 -> DependencyContext2 m
mkAdjCon2 fi fc event1 = M3 ((fi $ pos), (fi$ label), (fi $ mkAdjCon1 fi fc))
    where DE1 (pos, label) = event1

newtype DC2 = DC2 (DependencyContext2 Identity)
    deriving (Eq, Ord, Show, Binary)

type DependencySubContext2 = DependencyContext2 Maybe

instance Context DC2 where 
    type SubMap DC2 = M.Map
    type Sub DC2  = DependencySubContext2
    decompose (DC2 (M3 (a,b,c))) = map M3 
        [(ji a,n,n),
         (n,ji b,Just d1),
         (n,n, Just d3)]
            where [d1, _, d3] = decompose $ DC1 $ runIdentity c 


data FirstOrderDep = FirstOrderDep 
instance JointModel FirstOrderDep  where 
     data Pairs FirstOrderDep  =  
         Pairs {probInfo :: ((DE1, DC1),
                             (DE2, DC2)),
                decisionInfo ::  (Int, Int)}
         deriving (Eq, Ord)

     newtype Observation FirstOrderDep = 
         Observation (CondObserved (DE1) (DC1),
                      CondObserved (DE2) (DC2))
         deriving (Monoid, Show, Binary)

     data Probs FirstOrderDep = 
         Probs (CondDistribution (DE1) (DC1),
                CondDistribution (DE2) (DC2))


     data FullEvent FirstOrderDep   = DepFullEvent {
      childWord  :: AWord,
      childPOS   :: APOS,
      childLabel :: ALabel,
      childInd   :: Int -- Not included in prob
    }
 
     data FullContext FirstOrderDep =  DepFullContext { 
      adjSide    :: AdjunctionSide, 
      crossesVerb :: VerbDistance, 
      delta      :: Delta,
      parentPOS  :: APOS,
      parentWord :: AWord,
      parentInd  :: Int -- not included in prob
    } 
     
     
     chainRule fullEvent fullContext = 
         Pairs ((e1, DC1 $ mkAdjCon1 Identity fullContext),
                (e2, DC2 $ mkAdjCon2 Identity fullContext e1))
               (childInd fullEvent, parentInd fullContext)  
         where e1 = mkEvent1 fullEvent
               e2 = mkEvent2 fullEvent
                    
     observe (Pairs (a,b) _) = 
         Observation $ (((uncurry condObservation) a), ((uncurry condObservation) b))

     prob (Probs (p1,p2)) (Pairs (a,b) _) =           
         ((uncurry $ flip p1) a) * ((uncurry $ flip p2) b)

     estimate (Observation (!obs1,  !obs2)) = 
         Probs (est $! obs1, est $! obs2) 
         where  
           est :: (Event a, Context b) => CondObserved a b -> CondDistribution a b 
           est = mkDist . estimateGeneralLinear (wittenBell 5)


