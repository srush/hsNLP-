{-# LANGUAGE TypeSynonymInstances, TypeFamilies, FlexibleInstances, DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Adjunction (
                   TAGCounts,
                   TAGProbs,
                   Adjunction,
                   mkAdjunction,
                   countAdjunction,
                   probAdjunction,
                   estimateTAGProb
                  ) where 

import NLP.Probability.ConditionalDistribution
import NLP.Probability.Distribution 
import TAG
import Data.DeriveTH hiding (Derivation)
import Data.Binary hiding (Word)
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
-- Code for dealing with stocastic adjunction 
import Debug.Trace
import Control.Exception

between a b = [(min a b) +1  .. (max a b) -1] 
tabSep a b = a <> (text "\t")  <> b

instance (Enum a) => Enum (Maybe a) where 
    fromEnum Nothing = 0
    fromEnum (Just a) = 1 + fromEnum a 
    toEnum 0 = Nothing
    toEnum n = Just $ toEnum (n -1)
 
data AdjunctionEvent1 = AdjunctionEvent1 {
      childPOS   :: POS,
      childTopNT :: NonTerm, 
      isSister   :: AdjunctionType
    } deriving (Eq, Ord, Show)
$( derive makeBinary ''AdjunctionEvent1 )

instance Pretty AdjunctionEvent1 where 
    pPrint e1 = (pPrint $ childPOS e1) <+>
                (pPrint $ childTopNT e1) <+>
                (pPrint $ isSister e1)

instance Enum AdjunctionEvent1 where 
    fromEnum (AdjunctionEvent1 a b c) = mkFromEnum3 (a, maxBound) (b, maxBound) (c, maxBound) 
    toEnum n = AdjunctionEvent1 a b c 
        where (a,b,c) = mkToEnum3 (maxBound, maxBound, maxBound) n


type MAdjEvent1 = Maybe AdjunctionEvent1
                 
newtype AdjunctionEvent2 = AdjunctionEvent2 {
      childWord :: Word
} deriving (Eq, Ord, Show, Enum, Bounded)

$( derive makeBinary ''AdjunctionEvent2 )

instance Pretty AdjunctionEvent2 where 
    pPrint e2 = pPrint $ childWord e2 


type MAdjEvent2 = Maybe AdjunctionEvent2

newtype AdjunctionEvent3 = AdjunctionEvent3 {
      spine :: Spine
} deriving (Eq, Ord, Show, Enum)

instance Pretty AdjunctionEvent3 where 
    pPrint e3 = pPrint $ spine e3 

$( derive makeBinary ''AdjunctionEvent3 )

type MAdjEvent3 = Maybe AdjunctionEvent3

dec = Just 
data Distance = Distance { isAdjacent :: Bool,
                           spansVerb  :: Bool} 
    deriving (Show, Eq, Ord)
$( derive makeBinary ''Distance )
instance Pretty Distance where 
    pPrint d = text $ showBoolPair (isAdjacent d, spansVerb d)
        where
          showBool True = "1"
          showBool False = "0"
          showBoolPair :: (Bool, Bool) -> String
          showBoolPair (a,b) = printf "(%s %s)" (showBool a) (showBool b) 

data AdjunctionContext1 = 
    AdjunctionContext1 {
      parentNT   :: NonTerm, -- The nonterminal adjoining into 
      parentWord :: Word, -- the word of the head spine
      delta      :: Distance, -- The distance between the lexical items 
      parentPOS  :: POS, -- , -- pos of the word head spine
      headNT     :: NonTerm -- The nonterminal below where we adjoin
    } deriving (Eq, Ord, Show)

data AdjunctionSubContext1 = 
    AdjunctionSubContext1 {
      mparentNT   :: Maybe NonTerm, 
      mdelta      :: Maybe Distance,
      mparentPOS  :: Maybe POS, 
      mparentWord :: Maybe Word,
      mheadNT     :: Maybe NonTerm 
    } deriving (Eq, Ord, Show)

pPrintJust Nothing = empty 
pPrintJust (Just p) = pPrint p

instance Pretty AdjunctionSubContext1 where 
    pPrint s1 = (pPrintJust $ mparentNT s1) <+>
                (pPrintJust $ mdelta s1) <+>
                (pPrintJust $ mparentPOS s1) <+>
                (pPrintJust $ mparentWord s1) <+>
                (pPrintJust $ mheadNT s1) 
                


$( derive makeBinary ''AdjunctionSubContext1 )
type AdjContext1 = AdjunctionContext1

adjCon1Def = AdjunctionSubContext1 Nothing Nothing Nothing Nothing Nothing

instance Context AdjunctionContext1 where
    type Sub (AdjunctionContext1) = AdjunctionSubContext1
    decompose adjcon = 
        [adjCon1Def {mparentNT   = dec $ parentNT adjcon,
                     mheadNT     = dec $ headNT adjcon,
                     mdelta      = dec $ delta adjcon},
         adjCon1Def {mparentPOS  = dec $ parentPOS adjcon},
         adjCon1Def {mparentWord = dec $ parentWord adjcon}]


data AdjunctionContext2 = 
    AdjunctionContext2 {
      adjCon1 :: AdjunctionContext1,
      sc2ChildPOS :: Maybe POS, 
      sc2ChildNonTerm ::  Maybe NonTerm
    } deriving (Eq, Ord, Show)



data AdjunctionSubContext2 = 
    AdjunctionSubContext2 {
      msc2ChildPOS :: Maybe (Maybe POS), 
      msc2ChildNonTerm :: Maybe (Maybe NonTerm),
      madjCon1 ::  Maybe AdjunctionSubContext1
    } deriving (Eq, Ord, Show)

instance Pretty AdjunctionSubContext2 where 
    pPrint s2 = (pPrintJust $ msc2ChildPOS s2) <+>
                (pPrintJust $ msc2ChildNonTerm s2) <+>
                (pPrintJust $ madjCon1 s2)  


$( derive makeBinary ''AdjunctionSubContext2 )
adjCon2Def = AdjunctionSubContext2 Nothing Nothing Nothing

type AdjContext2 = AdjunctionContext2



instance Context (AdjunctionContext2 ) where
    type Sub (AdjunctionContext2) = AdjunctionSubContext2 
    decompose adjcon = 
        [adjCon2Def {
           msc2ChildPOS  = dec $ sc2ChildPOS adjcon
         },
         adjCon2Def { 
           msc2ChildNonTerm  = dec $ sc2ChildNonTerm adjcon,
           madjCon1 = dec $ decon11 {mparentPOS  = dec $ parentPOS $ adjCon1 adjcon}
         },
         adjCon2Def {madjCon1 = dec $ decon13}]
        where [decon11, _, decon13] = decompose $ adjCon1 adjcon

data AdjunctionContext3 = 
    AdjunctionContext3 {
      adjEvent13 :: MAdjEvent1,
      adjEvent23 :: MAdjEvent2,
      adjCon13 :: AdjunctionContext1
    } deriving (Eq, Ord, Show)


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
    decompose adjcon = 
        [adjCon3Def {madjEvent13 = dec $ adjEvent13 adjcon},
         adjCon3Def {madjEvent23 = dec $ adjEvent23 adjcon},         
         adjCon3Def {madjCon13 = dec $ decon11}
         ]
        where [decon11, _, decon13] = decompose $ adjCon13 adjcon



type AdjunctionObservations = 
    (CondObserved MAdjEvent1 AdjContext1,
     CondObserved MAdjEvent2 AdjContext2,
     CondObserved MAdjEvent3 AdjContext3)
                  

type AdjunctionDist = 
    (CondDistribution MAdjEvent1 AdjContext1,
     CondDistribution MAdjEvent2 AdjContext2,
     CondDistribution MAdjEvent3 AdjContext3)


mkAdjunctionEvents :: Maybe TAGWord -> AdjunctionType -> (MAdjEvent1,
                                                          MAdjEvent2,
                                                          MAdjEvent3)
mkAdjunctionEvents Nothing _ = (Nothing, Nothing, Nothing)
mkAdjunctionEvents (Just child) sister = 
  (Just $ AdjunctionEvent1
         childPOS -- r 
         (fromJustDef (fromPOS childPOS) $ top childSpine) -- R,  
         sister, -- sister,
   Just $ AdjunctionEvent2 childWord,
   Just $ AdjunctionEvent3 childSpine)
  where 
    (TAGWord (childWord, childPOS) childSpine _ _) = child


mkAdjunctionContexts :: 
    (MAdjEvent1,
     MAdjEvent2,
     MAdjEvent3) ->
    TAGWord -> 
    Maybe TAGWord -> 
    Int ->
    Distance -> 
    ((MAdjEvent1, AdjContext1),
     (MAdjEvent2, AdjContext2),
     (MAdjEvent3, AdjContext3))
mkAdjunctionContexts (e1, e2, e3)
                    (TAGWord (headWord, headPOS) headSpine _ _)
                    child
                    pos
                    delta =
    ((e1, adjcon1),
     (e2, AdjunctionContext2 adjcon1 (childPOS `liftM` e1) (childTopNT `liftM` e1) ),
     (e3, AdjunctionContext3 e1 e2 adjcon1))
      
          where adjcon1 = AdjunctionContext1 
                          (getNonTerm pos headSpine)
                          ( headWord)
                          ( delta)
                          ( headPOS)
                          ( fromJustDef (fromPOS headPOS) $ 
                                  lookupNonTerm (pos-1) headSpine) -- H
                          
data TAGCounts = 
    TAGCounts  {
      leftAdjCounts  :: AdjunctionObservations,
      rightAdjCounts ::AdjunctionObservations 
    } 
    deriving Eq


instance Monoid TAGCounts  where 
    mempty = TAGCounts  mempty mempty
    mappend (TAGCounts a b ) (TAGCounts a' b' ) = 
        TAGCounts (mappend a a') (mappend b b')


data TAGProbs = TAGProbs
    { leftProbs  :: AdjunctionDist,
      rightProbs  :: AdjunctionDist
    } 


estimateTAGProb :: TAGCounts -> TAGProbs
estimateTAGProb (TAGCounts (a,b,c) (a', b', c')) = 
    TAGProbs ( estimateWittenBell a, 
               estimateWittenBell b, 
               estimateWittenBell c)
             ( estimateWittenBell a', 
               estimateWittenBell b', 
               estimateWittenBell c')

instance Pretty TAGCounts where 
    pPrint (TAGCounts b c) = 
        vcat [(text "Left:  ") $$  (pPrint b), 
              (text "Right: ") $$  (pPrint c)]


instance Show TAGCounts where 
    show = render . pPrint 

instance Binary TAGCounts where 
    put ttc = (put $ leftAdjCounts ttc) >> 
              (put $ rightAdjCounts ttc)
    get = return TAGCounts `ap` get `ap` get 



-- exposed methods 


type Adjunction = (AdjunctionSide,
                   ((MAdjEvent1, AdjContext1),
                    (MAdjEvent2, AdjContext2),
                    (MAdjEvent3, AdjContext3)))

countAdjunction :: Adjunction  -> TAGCounts 
countAdjunction (side, ((e1,c1),(e2,c2), (e3,c3))) = 
    case side of 
      ARight -> mempty { rightAdjCounts = obs}
      ALeft  -> mempty { leftAdjCounts = obs}
    where obs = (singletonObservation e1 c1,
                 singletonObservation e2 c2,
                 singletonObservation e3 c3)
                 

probAdjunction :: Adjunction -> TAGProbs -> Prob 
probAdjunction  (side, ((e1,c1),(e2,c2), (e3,c3))) probs =
    res --trace ((show e1) ++ (show c1) ++ (show pr1) ++ (show res)) $ assert (not $ isNaN res) $  res 
        where (p1,p2,p3) = case side of 
                          ARight -> rightProbs probs
                          ALeft -> leftProbs probs
              pr1 = prob (cond p1 c1) e1 
              pr2 = prob (cond p2 c2) e2 
              pr3 = prob (cond p3 c3) e3 
              res = pr1 * pr2 * pr3 

mkAdjunction :: 
    TAGWord -> 
    Maybe TAGWord ->
    Int ->
    AdjunctionType ->
    AdjunctionSide ->
    Bool -> 
    Bool -> 
    Adjunction
mkAdjunction head child pos sister side isAdjacent isVerb  =
    (side,
     mkAdjunctionContexts events head child pos (Distance isAdjacent isVerb))
    where events = mkAdjunctionEvents child sister
   
getCounts f = do
    counts <- decodeFile f 
    return (counts :: TAGCounts)

getProbs c = 
    estimateTAGProb c 
