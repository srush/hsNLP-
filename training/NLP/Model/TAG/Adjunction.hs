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
import NLP.Grammar.Dependency
import Text.Printf
import NLP.Probability.Chain
import qualified NLP.Probability.SmoothTrie as ST
import NLP.Model.TAG.Wrap
import Control.DeepSeq
import NLP.Atom
import NLP.ParseMonad
import NLP.Grammar.Dependency
--}}}

emptyAdjunction = AdjunctionFullEvent Nothing Nothing Nothing Nothing Nothing Nothing Nothing


n = Nothing 
ji = Just . runIdentity
i = Identity

-- Adjunction Context1

type AdjunctionContext1 m =   
    (M7 m (ANonTerm) (Maybe ANonTerm) AdjunctionSide VerbDistance Delta (APOS) (AWord) )

addPOS (M7 (a,b,c,d,e,_,g)) (M7 (_,_,_,_,_,pos,_) )  = M7 (a,b,c,d,e,pos,g) 

--{{{  AdjunctionContext1 Classes
newtype AC1 = AC1 (AdjunctionContext1 Identity)
    deriving (Eq, Ord, Show, Binary)

type AdjunctionSubContext1 = AdjunctionContext1 Maybe

mkAdjCon1 ::(forall a. a -> m a) -> FullContext (Collins) -> AdjunctionContext1 m 
mkAdjCon1 fi (AdjunctionFullContext a b c d e f g _ _ _ _ _ _ ) = 
    M7 (fi a,fi b,fi c,fi d, fi e, fi f, fi g) 
       
instance Context (AC1 ) where 
    type SubMap (AC1) = M.Map
    type Sub AC1  = AdjunctionSubContext1
    decompose (AC1 (M7 (a,b,c,d,e,f,g))) = map M7 
        [(ji a,ji b,ji c,ji d,ji e,n,n),
         (n,n,n,n,n,ji f,n),
         (n,n,n,n,n,n,ji g)]
    --mShow = ac1ToReadable

newtype AC1Read = AC1Read (M7 Maybe NonTerm (Maybe NonTerm) AdjunctionSide VerbDistance Delta POS Word)

instance Pretty AC1Read where 
    pPrint (AC1Read (M7 (pnt, cnt, side, vd, delt,pos,word) )) = 
        ssep $ 
        [hPretty pnt,
         holderPretty (maybe (text "*") pPrint) cnt,
         hPretty side,
         hPretty vd,
         hPretty delt,
         hPretty pos,
         hPretty word]
         

ac1ToReadable :: AdjunctionSubContext1 -> ParseMonad AC1Read
ac1ToReadable (M7 (ant, mant, aside, vd, delta, apos, aword)) = do
  nt <- unAtom ant
  mnt <- unAtom mant
  word <- unAtom aword
  pos <- unAtom apos
  return $ AC1Read $ M7 (nt, mnt, aside, vd, delta, pos, word)
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

newtype AE1Read = AE1Read (Maybe POS, Maybe (Maybe NonTerm), Maybe AdjunctionType) 

ae1ToReadable ::AE1 -> ParseMonad AE1Read
ae1ToReadable (AE1 (apos, ammnt, atype)) = do
  pos <- unAtom apos
  mmnt <- unAtom ammnt
  return $ AE1Read (pos, mmnt, atype)

instance Pretty(AE1Read) where 
     pPrint (AE1Read (p, topNT, at)) = case p of 
                              Nothing -> text "END"
                              Just _ -> ssep [hPretty p,
                                              holderPretty (maybe (text "*") pPrint)  topNT, 
                                              hPretty at] 

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
    type SubMap AC2 = M.Map
    type Sub AC2  = AdjunctionSubContext2
    decompose (AC2 (M3 (a,b,c))) = map M3 
        [(ji a,n, n),
         (n, ji b, Just $ addPOS d1 d2),
         (n, n, Just d3)]
            where [d1, d2, d3] = decompose $ AC1 $ runIdentity c 

-- type AdjunctionContext2 m = M3 m (Maybe (APOS)) (AE1 ) (AdjunctionContext1 m )

newtype AC2Read = AC2Read (M3 Maybe (Maybe POS) AE1Read AC1Read)

instance Pretty AC2Read where 
    pPrint (AC2Read (M3 (pos, ae1, ac1) )) = 
        ssep $ 
        [holderPretty (maybe (text "END") pPrint)  pos,
         hPretty ae1,
         hPretty ac1]
         


ac2ToReadable :: AdjunctionSubContext2 -> ParseMonad AC2Read
ac2ToReadable (M3 (mapos, ae1, ac1)) = do
  mpos <- unAtom mapos
  ae1r <- maybe (return Nothing) (\a -> Just `liftM` ae1ToReadable a)  ae1
  ac1r <- maybe (return Nothing) (\a -> Just `liftM` ac1ToReadable a) ac1
  return $ AC2Read $ M3 (mpos, ae1r, ac1r)

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

newtype AE2Read = AE2Read (Maybe Word) 

instance Pretty (AE2Read) where 
    pPrint (AE2Read a) = case a of
                       Just _ -> hPretty a
                       Nothing -> text "END"

ae2ToReadable ::AE2 -> ParseMonad AE2Read
ae2ToReadable (AE2 (aword)) = do
  word <- unAtom aword
  return $ AE2Read (word)

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

--type AdjunctionContext3 m = M3 m (AE1 ) (AE2) (AdjunctionContext1 m)
newtype AC3Read = AC3Read (M3 Maybe AE1Read AE2Read AC1Read)
    deriving (Pretty)


ac3ToReadable :: AdjunctionSubContext3 -> ParseMonad AC3Read
ac3ToReadable (M3 (ae1,ae2,ac1)) = do
  ae1r <- maybe (return Nothing) (\a -> Just `liftM` ae1ToReadable a) ae1
  ae2r <- maybe (return Nothing) (\a -> Just `liftM` ae2ToReadable a) ae2
  ac1r <- maybe (return Nothing) (\a -> Just `liftM` ac1ToReadable a) ac1
  return $ AC3Read $ M3 (ae1r, ae2r, ac1r)

--}}}

newtype AE3 = AE3 (Maybe (ASpine))
    deriving (Eq, Ord, Show, Binary)

--{{{  AdjunctionEvent3 Classes
mkEvent3 event = AE3 (atomChildSpine event) 

instance Event (AE3) where type EventMap (AE3) = M.Map

instance Pretty (AE3) where 
    pPrint (AE3 a) = hPretty a 

newtype AE3Read = AE3Read (Maybe RSpine) 

instance Pretty (AE3Read) where 
    pPrint (AE3Read a) =  (maybe (text "END") pPrint) a 

ae3ToReadable ::AE3 -> ParseMonad AE3Read
ae3ToReadable (AE3 (aspine)) = do
  spine <- unAtom aspine
  return $ AE3Read spine


--}}}

data Collins = Collins

-- dumpCollinsObs :: Observation Collins -> ParseMonad Doc
-- dumpCollinsObs (Observation (o1,o2,o3)) = do 
--   d1 <- showPretty (const $ return empty) t (o1::CondObserved AE1 AC1) --  (\a -> do {b<- ae1ToReadable a;return $ pPrint b}) (\a -> do {b<- ac1ToReadable a;return $ pPrint b}) o1  
--   --d2 <- showPretty (pP ae2ToReadable) (pP ac2ToReadable) o2 
--   --d3 <- showPretty (pP ae3ToReadable) (pP ac3ToReadable) o3 
--   return $ vcat [d1]
--       where 
--         t :: (Sub AC1 -> ParseMonad Doc)
--         t = (const $ return empty)
--     --     pP ::  (forall a.forall b .(Pretty b) => a -> ParseMonad b) -> a -> ParseMonad Doc
-- --         pP f a= do
-- --               b <- f a
-- --               return $ pPrint b

ssep = hcat .  punctuate (text " ") . filter (not. isEmpty) 

dumpCollinsObs :: Observation Collins  ->  ParseMonad Doc 
dumpCollinsObs (Observation (o1,o2,o3)) =do
    d1 <- showPretty o1 (liftM pPrint .  ae1ToReadable) (liftM pPrint . ac1ToReadable)
    d2 <- showPretty o2 (liftM pPrint .  ae2ToReadable) (liftM pPrint . ac2ToReadable)
    d3 <- showPretty o3 (liftM pPrint .  ae3ToReadable) (liftM pPrint . ac3ToReadable)
    return $ hcat [d1,d2,d3]
    where showPretty t mEventShow mShow = ST.mPretty (showObsPretty mEventShow) (\a -> do { a <- mapM mShow a; return $ hcat a}) t
   
dumpPairs :: Pairs Collins -> ParseMonad Doc
dumpPairs pairs = do
  dae1 <- ae1ToReadable ae1
  dae2 <- ae2ToReadable ae2
  dae3 <- ae3ToReadable ae3
  dac1 <- mapM ac1ToReadable $ decompose ac1
  dac2 <- mapM ac2ToReadable $ decompose ac2
  dac3 <- mapM ac3ToReadable $ decompose ac3
  return $ hang (pPrint dac1) 2 (pPrint dae1) $$
           hang (pPrint dac2) 2 (pPrint dae2) $$
           hang (pPrint dac3) 2 (pPrint dae3)
    where ((ae1, ac1),
           (ae2, ac2),
           (ae3, ac3)) = probInfo pairs 

instance JointModel (Collins) where 
     data Pairs (Collins) =  
         Pairs {probInfo :: ((AE1, AC1),
                             (AE2, AC2),
                             (AE3, AC3)),
                decisionInfo :: Maybe (Int, Int), -- Child, Head (m, h)
                makesBaseNP :: (Bool, (Int, Int)), -- (is base np, covers)
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
    } deriving (Eq, Show)
 
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
      parentTWord :: TWord, -- Not included
      -- weirder things
      prevRegular :: Bool, -- Not included
      splitPoint  :: Int, -- Not included
      inNPB       :: Bool -- Not included
    } deriving (Show,Eq)
     
     
     chainRule fullEvent fullContext = 
         Pairs ((e1, AC1 $ mkAdjCon1 Identity fullContext),
                (e2, AC2 $ mkAdjCon2 Identity fullContext e1),
                (e3, AC3 $ mkAdjCon3 Identity fullContext e1 e2))
               (fmap (\cInd -> (cInd, parentInd fullContext)) $ childInd fullEvent)
               (inNPB fullContext, sortPair (parentInd fullContext, splitPoint fullContext))
               -- if pos of spine should be NPB and word is end then NPB 
               -- else split
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
               sortPair (a,b) = (min a b, max a b)
                    
     observe (Pairs {probInfo = (a,b,c)}) = 
         Observation $ (((uncurry condObservation) a), ((uncurry condObservation) b), ((uncurry condObservation) c))

     prob (Probs (p1,p2,p3)) (Pairs {probInfo = (a,b,c)}) =           
         ((uncurry $ flip p1) a) * ((uncurry $ flip p2) b) * ((uncurry $ flip p3) c)

     estimate (Observation (!obs1,  !obs2,  !obs3)) = 
         Probs (est $! obs1, est $! obs2, est $! obs3) 
         where  
           est :: (Event a, Context b) => CondObserved a b -> CondDistribution a b 
           est = mkDist . estimateGeneralLinear (wittenBell 5)


estimateDebug (Observation (!obs1,  !obs2,  !obs3)) = 
    (est $! obs1, est $! obs2, est $! obs3) 
        where  
           est :: (Event a, Context b) => CondObserved a b -> DebugDist a b 
           est =  estimateGeneralLinear (wittenBell 5)

probDebug :: ProbsDebug -> Pairs Collins -> ProbDebug
probDebug (p1,p2,p3) (Pairs {probInfo = (a,b,c)}) =           
         ProbDebug [((uncurry $ flip p1) a), ((uncurry $ flip p2) b), ((uncurry $ flip p3) c)]

type ProbsDebug = (DebugDist AE1 AC1,
                   DebugDist AE2 AC2,
                   DebugDist AE3 AC3)
 
newtype ProbDebug = ProbDebug ([[(Double,Double)]])
    deriving (Eq, Show)
instance Pretty ProbDebug where 
    pPrint (ProbDebug ls ) = 
        vcat 
        [(hcat $ punctuate (text " + ")  
         [text $ printf "%.3e * %.3e" lambda p  
         |  
          (p, lambda) <- prob]) <+> 
         (text " = ") <+> (text  (printf "%.3e"  (sum $ map (uncurry (*)) prob)))
         | prob <- ls]
                               