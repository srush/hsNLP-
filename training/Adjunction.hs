{-# LANGUAGE TypeSynonymInstances, TypeFamilies, FlexibleInstances, DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Adjunction (
                   TAGCounts,
                   TAGProbs,
                   Adjunction,
                   AdjunctionParent,
                   mkAdjunction,
                   countAdjunction,
                   probAdjunction,
                   estimateTAGProb,
                   mkParent,
                   ProbDebug,
                   probAdjunctionDebug, 
                   Distance(..),                   
                   AdjunctionAction(..),
                   maybeAdj
                  ) where 
import Data.Function (on)
import Data.Maybe (isJust)

import NLP.Probability.ConditionalDistribution
import NLP.Probability.Distribution 
import TAG
import Data.DeriveTH hiding (Derivation)
import Data.Binary hiding (Word)
import Data.List (intercalate)
import Control.Monad (liftM, ap)
import Safe (fromJustNote, fromJustDef) 
import Sentence
import Text.PrettyPrint.HughesPJClass
import Control.Monad.Identity
import Data.Monoid
import POS
import NonTerm
import EnumHelpers
import Word
import Text.Printf
import Distance
import qualified Data.Map as M
import qualified Data.ListTrie.Base.Map as LT
import qualified Data.Bimap as BM 
-- Code for dealing with stocastic adjunction 
import Debug.Trace
import Control.Exception
import Test.QuickCheck
import qualified Punctuation as P
data EnumCached a = 
    EnumCached { enumVal :: a, 
                 enumInd :: Int}
                  
cacheEnum a = EnumCached a $ fromEnum a


instance Enum a => Enum (EnumCached a) where 
    fromEnum (EnumCached _ n) = n 
    toEnum n = (EnumCached (toEnum n) n) 

instance Eq (EnumCached a) where 
    (==) = (==) `on` enumInd

instance Ord (EnumCached a) where 
    compare = compare `on` enumInd
instance Show a => Show (EnumCached a) where 
    show = show . enumVal

instance Pretty a => Pretty (EnumCached a) where 
    pPrint e = (pPrint $ enumVal e) 
 
instance (Binary a, Enum a) => Binary (EnumCached a) where 
    put ttc = (put $ enumVal ttc)
    get = do 
      v <- get 
      return $ cacheEnum v

between a b = [(min a b) +1  .. (max a b) -1] 
tabSep a b = a <> (text "\t")  <> b

instance (Enum a) => Enum (Maybe a) where 
    {-# INLINE toEnum #-}
    fromEnum Nothing = 0
    fromEnum (Just a) = 1 + fromEnum a 
    toEnum 0 = Nothing
    toEnum n = Just $ toEnum (n -1)
 
data AdjunctionEvent1 = AdjunctionEvent1 {
      childPOS   :: POS,
      childTopNT :: NonTerm,
      isSister   :: AdjunctionType,
      punc :: Bool
    } deriving (Eq, Ord, Show)
$( derive makeBinary ''AdjunctionEvent1 )

instance Pretty AdjunctionEvent1 where 
    pPrint e1 = (pPrint $ childPOS e1) <+>
                (pPrint $ childTopNT e1)  <+>
                (pPrint $ isSister e1) <+>
                (if punc e1 then text "Punc" else empty )

instance Enum AdjunctionEvent1 where 
    fromEnum (AdjunctionEvent1 a b c d) = mkFromEnum4 (a, maxBound) (b, maxBound) (c, maxBound) (d, maxBound) 
    toEnum n = AdjunctionEvent1 a b c d
        where (a,b,c,d) = mkToEnum4 (maxBound, maxBound, maxBound, maxBound) n



data AdjunctionAction a = End | DoAdj a
                        deriving (Eq, Ord, Show)

maybeAdj End = Nothing 
maybeAdj (DoAdj a) = Just a 

instance (Pretty a) => (Pretty (AdjunctionAction a)) where 
    pPrint (End) = text "END"
    pPrint (DoAdj a) = pPrint a  

instance (Functor AdjunctionAction) where 
    fmap f End = End
    fmap f (DoAdj a) = DoAdj $ f a

$( derive makeBinary ''AdjunctionAction)
$( derive makeArbitrary ''AdjunctionAction)


instance (Enum a) => Enum (AdjunctionAction a) where 
    {-# INLINE toEnum #-}
    fromEnum End = 0
    fromEnum (DoAdj a) = 1 + fromEnum a 
    toEnum 0 = End
    toEnum n = DoAdj $ toEnum (n - 1)


type MAdjEvent1 = AdjunctionAction AdjunctionEvent1
                 
newtype AdjunctionEvent2 = AdjunctionEvent2 {
      childWord :: Word
} deriving (Eq, Ord, Show, Enum, Bounded)

$( derive makeBinary ''AdjunctionEvent2 )

instance Pretty AdjunctionEvent2 where 
    pPrint e2 = pPrint $ childWord e2 


type MAdjEvent2 = AdjunctionAction AdjunctionEvent2

newtype AdjunctionEvent3 = AdjunctionEvent3 {
      spine :: Spine
} deriving (Eq, Ord, Show, Enum)

instance Pretty AdjunctionEvent3 where 
    pPrint e3 = pPrint $ spine e3 

$( derive makeBinary ''AdjunctionEvent3 )

type MAdjEvent3 = AdjunctionAction AdjunctionEvent3

dec = Just 

--data AdjunctionContext1 = 
--    AdjunctionContext1 {
--      parentNT   :: NonTerm, -- The nonterminal adjoining into 
--      parentWord :: Word, -- the word of the head spine
--      delta      :: Distance, -- The distance between the lexical items 
--      parentPOS  :: POS, -- , -- pos of the word head spine
--      headNT     :: NonTerm -- The nonterminal below where we adjoin
--    } deriving (Eq, Ord, Show)

data AdjunctionSubContext1 = 
    AdjunctionSubContext1 {
      mparentNT   :: Maybe NonTerm, 
      mdelta      :: Maybe Distance,
      mside       :: Maybe AdjunctionSide,
      --mtype       :: Maybe AdjunctionType,
      mparentPOS  :: Maybe POS, 
      mparentWord :: Maybe Word,
      mheadNT     :: Maybe NonTerm
    } deriving (Eq, Ord, Show)

instance Enum AdjunctionSubContext1 where 
    {-# INLINE fromEnum #-}
    fromEnum asc = mkFromEnum6 (mparentNT asc, Just maxBound) 
                               (mdelta asc,    Just maxBound)
                               (mside asc,     Just maxBound)                         
                               --(mtype asc,     Just maxBound)                         
                               (mparentPOS asc,  Just maxBound)
                               (mparentWord asc, Just maxBound)
                               (mheadNT asc,   Just maxBound)
                         
    toEnum n = AdjunctionSubContext1 a b c d e f
        where (a,b,c,d,e,f) = mkToEnum6 (Just maxBound, Just maxBound, Just maxBound, Just maxBound, Just maxBound, Just maxBound) n

pPrintJust Nothing = empty 
pPrintJust (Just p) = pPrint p

instance Pretty AdjunctionSubContext1 where 
    pPrint s1 = 
        (pPrintJust $ mside s1) <+>
        (pPrintJust $ mparentNT s1) <+>
        --(pPrintJust $ mtype s1) <+>
        (pPrintJust $ mdelta s1) <+>
                (pPrintJust $ mparentPOS s1) <+>
                (pPrintJust $ mparentWord s1) <+>
                (pPrintJust $ mheadNT s1) 
                


$( derive makeBinary ''AdjunctionSubContext1 )
$( derive makeArbitrary ''AdjunctionSubContext1 )
type AdjContext1 = AdjunctionContext1


adjCon1Def = AdjunctionSubContext1 Nothing Nothing Nothing Nothing Nothing Nothing
type AdjunctionContext1 = [EnumCached AdjunctionSubContext1]

prop_context1 a = checkEnum
    where types = (a::AdjunctionSubContext1) 


mkAdjCon1 parentNT headNT delta parentPOS parentWord side atype = 
        map cacheEnum [adjCon1Def {mparentNT   = dec $ parentNT,
                                   mheadNT     = dec $ headNT ,
                                   mdelta      = dec $ delta,
                                   mside        = dec $ side 
                                   --mtype        = dec $ atype
                                  },
                       adjCon1Def {mparentPOS  = dec $ parentPOS },
                       adjCon1Def {mparentWord = dec $ parentWord }]


instance Context AdjunctionContext1 where
    type Sub (AdjunctionContext1) = EnumCached AdjunctionSubContext1
    type SubMap (AdjunctionContext1) = LT.WrappedIntMap
    decompose = id

data AdjunctionContext2 = 
    AdjunctionContext2 {
      adjCon1 :: AdjunctionContext1,
      sc2ChildPOS :: Maybe POS, 
      sc2Side     :: AdjunctionSide,
      sc2ChildNonTerm ::  Maybe NonTerm
    } deriving (Eq, Ord, Show)

data AdjunctionSubContext2 = 
    AdjunctionSubContext2 {
      msc2ChildPOS :: Maybe (Maybe POS), 
      msc2AdjEvent :: Maybe (MAdjEvent1),
      madjCon1 ::  Maybe AdjunctionSubContext1
    } deriving (Eq, Ord, Show)

instance Pretty AdjunctionContext2 where 
    pPrint = pPrint . decompose

instance Pretty AdjunctionSubContext2 where 
    pPrint s2 = 

        (pPrintJust $ msc2ChildPOS s2) <+>
        (pPrintJust $ msc2AdjEvent1 s2) <+>
        (pPrintJust $ msc2ChildNonTerm s2) <+>
                (pPrintJust $ madjCon1 s2)  


$( derive makeBinary ''AdjunctionSubContext2 )
adjCon2Def = AdjunctionSubContext2 Nothing Nothing Nothing Nothing

type AdjContext2 = AdjunctionContext2



instance Context (AdjunctionContext2 ) where
    type Sub (AdjunctionContext2) = AdjunctionSubContext2 
    type SubMap (AdjunctionContext2) = M.Map 
    decompose adjcon = 
        [adjCon2Def {
           msc2ChildPOS  = dec $ sc2ChildPOS adjcon
         },
         adjCon2Def { 
           msc2Side  = dec $ sc2Side adjcon,
           msc2ChildNonTerm  = dec $ sc2ChildNonTerm adjcon,
           madjCon1 = dec $ (enumVal decon11) {mparentPOS  = mparentPOS $ enumVal $ decon12}
         },
         adjCon2Def {madjCon1 = dec $ enumVal decon13}]
        where [decon11, decon12, decon13] = decompose $ adjCon1 adjcon

data AdjunctionContext3 = 
    AdjunctionContext3 {
      adjEvent13 :: MAdjEvent1,
      adjEvent23 :: MAdjEvent2,
      adjCon13 :: AdjunctionContext1
    } deriving (Eq, Ord, Show)

instance Pretty AdjunctionContext3 where 
    pPrint = pPrint . decompose


data AdjunctionSubContext3 = 
    AdjunctionSubContext3 {
      madjEvent13 :: Maybe MAdjEvent1,
      madjEvent23 :: Maybe MAdjEvent2,
      madjCon13 :: Maybe (AdjunctionSubContext1)
    } deriving (Eq, Ord, Show)

$( derive makeBinary ''AdjunctionSubContext3 )

instance Pretty AdjunctionSubContext3 where 
    pPrint s3 = (pPrintJust $ madjEvent13 s3) <+>
                (pPrintJust $ madjEvent23 s3)  


type AdjContext3 = AdjunctionContext3  

adjCon3Def = AdjunctionSubContext3 Nothing Nothing Nothing

instance Context AdjunctionContext3  where
    type Sub AdjunctionContext3 = AdjunctionSubContext3
    type SubMap (AdjunctionContext3) = M.Map 
    decompose adjcon = 
        [adjCon3Def {madjEvent13 = dec $ adjEvent13 adjcon},
         adjCon3Def {madjEvent23 = dec $ adjEvent23 adjcon},         
         adjCon3Def {madjCon13 = dec $ enumVal $ decon11}
         ]
        where [decon11, _, decon13] = decompose $ adjCon13 adjcon



type AdjunctionObservations = 
    (CondObserved MAdjEvent1 AdjContext1,
     CondObserved MAdjEvent2 AdjContext2,
     CondObserved MAdjEvent3 AdjContext3,
     P.PunctuationObservations
    )
                  

type AdjunctionDist = 
    (CondDistribution MAdjEvent1 AdjContext1,
     CondDistribution MAdjEvent2 AdjContext2,
     CondDistribution MAdjEvent3 AdjContext3,
     P.PunctuationDistribution 
    )


mkAdjunctionEvents :: AdjunctionAction TAGWord -> AdjunctionType -> 
                       Maybe GWord -> (MAdjEvent1,
                                       MAdjEvent2,
                                       MAdjEvent3,
                                       Maybe P.PunctuationEvent)
mkAdjunctionEvents (DoAdj child) sister punc = 
  (DoAdj $ AdjunctionEvent1
         childPOS -- r 
         (fromJustDef (fromPOS childPOS) $ top childSpine) -- R,  
         sister
         $ isJust punc
  , -- sister,
   DoAdj $ AdjunctionEvent2 childWord,
   DoAdj $ AdjunctionEvent3 childSpine,
   P.mkPunctuationEvent `liftM` punc
  )
  where 
    (TAGWord childSpine (childWord, childPOS) _ _  _ _) = child
mkAdjunctionEvents End _ _ = (End, End, End, Nothing)



mkMainAdjunctionContext (TAGWord headSpine (headWord, headPOS) _ _ _ _) pos delta side atype =    mkAdjCon1 
    (getNonTerm pos headSpine)
    (fromJustDef (fromPOS headPOS) $ 
      lookupNonTerm (pos-1) headSpine) -- H
    (delta)
    (headPOS)
    (headWord)
    side
    atype


    
mkAdjunctionContexts ::
    AdjContext1 -> 
    (MAdjEvent1,
     MAdjEvent2,
     MAdjEvent3,
     Maybe P.PunctuationEvent) ->
    AdjunctionAction TAGWord ->
    AdjunctionSide ->
    ((MAdjEvent1, AdjContext1),
     (MAdjEvent2, AdjContext2),
     (MAdjEvent3, AdjContext3),
     Maybe (P.PunctuationEvent, P.PunctuationContext))
mkAdjunctionContexts adjcon1 (e1, e2, e3, mPevent) child side =
    ((e1, adjcon1),
     (e2, AdjunctionContext2 adjcon1 (childPOS `liftM` (maybeAdj e1)) side (childTopNT `liftM` (maybeAdj e1)) ),
     (e3, AdjunctionContext3 e1 e2 adjcon1),
     case mPevent of 
       Nothing -> Nothing
       (Just pevent) -> Just (pevent, mkPunctuationContext side (isSister $ fromJustNote "punc at top" (maybeAdj e1))  child adjcon1 )
    )
                               
data TAGCounts = 
    TAGCounts { tagCounts :: AdjunctionObservations}
    


instance Monoid TAGCounts  where 
    mempty = TAGCounts mempty
    mappend (TAGCounts a) (TAGCounts a') = 
        TAGCounts (mappend a a')


data TAGProbs = TAGProbs { tagProbs :: AdjunctionDist}
    


estimateTAGProb :: TAGCounts -> TAGProbs
estimateTAGProb (TAGCounts (a,b,c,d) ) = 
    TAGProbs ( estimateWittenBell a, 
               estimateWittenBell b, 
               estimateWittenBell c,
               estimateWittenBell d)

instance Pretty TAGCounts where 
    pPrint (TAGCounts b) = 
        vcat [pPrint b]


instance Show TAGCounts where 
    show = render . pPrint 

instance Binary TAGCounts where 
    put (TAGCounts ttc) = (put $  ttc)
    get = return TAGCounts `ap` get 



-- exposed methods 


type AdjunctionParent = (AdjunctionSide, AdjContext1)

type Adjunction = (AdjunctionSide,
                   ((MAdjEvent1, AdjContext1),
                    (MAdjEvent2, AdjContext2),
                    (MAdjEvent3, AdjContext3),
                    Maybe (P.PunctuationEvent, P.PunctuationContext)))

countAdjunction :: Adjunction  -> TAGCounts 
countAdjunction (side, ((e1,c1),(e2,c2), (e3,c3), punc)) = 
    TAGCounts obs 
    where obs = (singletonObservation e1 c1,
                 singletonObservation e2 c2,
                 singletonObservation e3 c3,
                 case punc of 
                   Nothing -> mempty
                   Just (e4, c4) -> singletonObservation e4 c4
                )
                 

probAdjunction :: Adjunction -> TAGProbs -> Prob 
probAdjunction  (_, ((e1,c1),(e2,c2), (e3,c3), punc)) probs =
    res --trace ((show e1) ++ (show c1) ++ (show pr1) ++ (show res)) $ assert (not $ isNaN res) $  res 
        where (p1,p2,p3,p4) = tagProbs probs
              pr1 = prob (cond p1 c1) e1 
              pr2 = prob (cond p2 c2) e2 
              pr3 = prob (cond p3 c3) e3 
              res = pr1 * pr2 * pr3 * 
                    case punc of 
                      Nothing -> 1.0
                      Just (e4, c4) -> prob (cond p4 c4) e4 

probAdjunctionDebug :: Adjunction -> TAGProbs -> ProbDebug 
probAdjunctionDebug  (_, ((e1,c1),(e2,c2), (e3,c3), punc)) probs =
    ProbDebug e1 e2 e3 c1 c2 c3 punc pr1 pr2 pr3 pr4 pr1' pr2' pr3' pr4' res
        where (p1,p2,p3, p4) = tagProbs probs
              pr1 = ProbsWithSmoothing  $ (condDebug p1 c1) e1 
              pr2 = ProbsWithSmoothing  $ (condDebug p2 c2) e2 
              pr3 = ProbsWithSmoothing  $ (condDebug p3 c3) e3
              pr4 = ProbsWithSmoothing $  case punc of 
                                           Nothing -> []
                                           Just (e4, c4) -> (condDebug p4 c4) e4
              pr1' = prob (cond p1 c1) e1 
              pr2' = prob (cond p2 c2) e2 
              pr3' = prob (cond p3 c3) e3
              pr4' = case punc of 
                                           Nothing -> 1.0
                                           Just (e4, c4) -> prob (cond p4 c4) e4
              res = pr1' * pr2' * pr3' * pr4'


newtype ProbsWithSmoothing = ProbsWithSmoothing [(Double, Double)]
    deriving Eq

instance Show ProbsWithSmoothing where 
    show (ProbsWithSmoothing pws) = intercalate " + " $ map (\(l,p) -> printf "%.3e * %.3e" l p) pws

instance Pretty ProbsWithSmoothing where 
    pPrint = text . show 


data ProbDebug = ProbDebug {
      de1 :: MAdjEvent1,
      de2 :: MAdjEvent2,
      de3 :: MAdjEvent3,
      dc1 :: AdjunctionContext1,
      dc2 :: AdjunctionContext2,
      dc3 :: AdjunctionContext3,
      puncy :: Maybe (P.PunctuationEvent, P.PunctuationContext),
      dp1 :: ProbsWithSmoothing,
      dp2 :: ProbsWithSmoothing,
      dp3 :: ProbsWithSmoothing,
      dp4 :: ProbsWithSmoothing,
      dpr1 :: Double,
      dpr2 :: Double,
      dpr3 :: Double,
      dpr4 :: Double,
      dres :: Double
    } deriving Eq


instance Show ProbDebug where 
    show = render . pPrint 


instance Pretty ProbDebug where 
    pPrint pd = 
        hang (text "Word Prob " <+> text (printf "%.3e" $ dres pd) ) 2 $  
          (hang ((pPrint $ de1 pd) <+> (pPrint $ dc1 pd)) 2  
           ((text $ printf "%.3e" $ dpr1 pd) <+> (text " = ") <+> (pPrint $ dp1 pd)) $$
           hang ((pPrint $ de2 pd) <+> (pPrint $ dc2 pd)) 2  
            ((text $ printf "%.3e" $ dpr2 pd) <+> (text " = ") <+> (pPrint $ dp2 pd)) $$
           hang ((pPrint $ de3 pd) <+> (pPrint $ dc3 pd)) 2  
            ((text $ printf "%.3e" $ dpr3 pd) <+> (text " = ") <+> (pPrint $ dp3 pd)) $$
           hang ((pPrint $ puncy pd)) 2  
            ((text $ printf "%.3e" $ dpr4 pd) <+> (text " = ") <+> (pPrint $ dp4 pd)) 

          ) 

mkParent:: TAGWord -> Int -> AdjunctionSide -> AdjunctionType ->     
           Distance ->

            AdjunctionParent
mkParent head pos side atype distance  = 
    (side, mkMainAdjunctionContext head pos distance side atype)

mkAdjunction :: 
    AdjunctionParent ->
    AdjunctionAction TAGWord ->
    AdjunctionType ->
    Maybe GWord ->
    Adjunction
mkAdjunction (side, adjcon1) child sister punc =
    (side, mkAdjunctionContexts adjcon1 events child side)
    where events = mkAdjunctionEvents child sister punc
   
getCounts f = do
    counts <- decodeFile f 
    return (counts :: TAGCounts)

getProbs c = 
    estimateTAGProb c 




mkPunctuationContext :: 
    AdjunctionSide -> AdjunctionType -> AdjunctionAction TAGWord -> AdjunctionContext1 -> P.PunctuationContext
mkPunctuationContext side atype child [sub1, sub2, sub3]   -- ((headWord, headPOS), parentNT, headNT) 
                            = 
    P.PunctuationContext [ P.puncSubDef {P.side      = mside $ enumVal (sub1) , 
                    P.headNT    = mheadNT $ enumVal (sub1),
                    P.childNT   = fromJustNote "exists" $ top `liftM` twSpine `liftM` child',
                    P.parentNT  = mparentNT $ enumVal (sub1),
                    P.atype     = Just atype
                                        },
      P.puncSubDef {P.parentPOS   = mparentPOS $ enumVal (sub2),
                    P.childPOS  = snd `liftM` twWord `liftM` child'},
      P.puncSubDef {P.parentWord  = mparentWord $ enumVal (sub3),
                    P.childWord  =fst `liftM` twWord `liftM` child'}
    ]

    where child' = maybeAdj child 