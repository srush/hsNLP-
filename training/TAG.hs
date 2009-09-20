{-# LANGUAGE TypeSynonymInstances, TypeFamilies, FlexibleInstances, DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module TAG where 
import Control.Monad (liftM, ap)
import qualified Sentence as S
import Sentence hiding (Word) 
import DependencyStructure
import Test.QuickCheck
import ArbitraryHelpers
import Data.Char (toUpper)
import Data.List
import Control.Monad (ap)
import Data.Maybe (fromJust)
import Data.Monoid
import NLP.Semiring
import NLP.Semiring.Derivation
import NLP.Semiring.Prob

import NLP.FSM.Simple
import NLP.Probability.ConditionalDistribution
import NLP.Probability.Distribution
import NLP.ChartParse
import NLP.ChartParse.Eisner
import Debug.Trace
import Debug.Trace.Helpers
import Data.Generics
import Control.Exception

import Text.PrettyPrint.HughesPJClass

import Test.HUnit

import Data.DeriveTH hiding (Derivation)
import Data.Binary

import qualified Data.Map as M
import qualified Data.Map as M


-- | A non-terminal in the TAG grammar 
newtype NonTerm = NonTerm String 
    deriving (Eq, Ord, Data, Typeable, Binary)


instance Show NonTerm where 
    show (NonTerm nt) = nt

instance Arbitrary NonTerm where
    arbitrary = 
      NonTerm `liftM` map toUpper `liftM` listOf1 (elements alpha) 


-- | There are two types of adjunction, 
--   Sister comes from a single position, 
--   regular duplicates the node in the head tree 
data AdjunctionType = Sister | Regular
    deriving  (Eq, Ord, Enum)

instance Show AdjunctionType where 
    show Sister = "s"
    show Regular = "a"

instance Arbitrary AdjunctionType where 
    arbitrary = elements [Sister, Regular]

$( derive makeBinary ''AdjunctionType ) 

  
newtype Spine = Spine [NonTerm]
    deriving (Eq, Ord, Data, Typeable, Binary)

top (Spine (nt:_)) = Just nt
top (Spine []) = Nothing

getNonTerm i (Spine nts) = nts !! i
        

type TAGWord = (GWord, Spine)

instance Context GWord where 
    type Sub (GWord) = String
    decompose (S.Word word, POS pos) = [pos, word] 
    compose [pos, word] = (S.Word word, POS pos)


type Distance = (Bool) -- TODO : this make this more accurate
type AdjunctionContext = 
    (NonTerm, -- The nonterminal adjoining into 
     Distance, -- The distance between the lexical items 
     POS, -- pos of the word head spine
     S.Word) -- the word of the head spine

-- this is lame, but need it to make subcontext type safe
data AdjunctionSub = AdjunctionSub {
      adjNonTerm :: Maybe NonTerm,
      adjDistance:: Maybe Distance,
      adjPOS :: Maybe POS,
      adjWord :: Maybe S.Word }
                     deriving (Eq, Ord)
$( derive makeBinary ''AdjunctionSub )

instance Show AdjunctionSub where 
    show a = concatMap (\f-> f a) 
             [maybeShow . adjNonTerm, 
              maybeShow. adjDistance,
              maybeShow. adjPOS, 
              maybeShow. adjWord]  
        where maybeShow Nothing = ""
              maybeShow (Just m) = show m ++ " " 

adjSubDef = AdjunctionSub Nothing Nothing Nothing Nothing

instance Context AdjunctionContext where
    type Sub (AdjunctionContext) = AdjunctionSub
    decompose (nonterm, dis, pos, word) = 
        --[adjSubDef {adjNonTerm =Just nonterm,
         --           adjDistance = Just dis},
         --adjSubDef {adjPOS = Just pos},
         --adjSubDef {adjWord = Just word}]
        [adjSubDef {adjNonTerm =Just nonterm},
         adjSubDef {adjDistance = Just dis},
         adjSubDef {adjPOS = Just pos}] -- simpler decomp
    compose [adjSub1,adjSub2,adjSub3] =  
        (fromJust $ adjNonTerm adjSub1, 
         fromJust $ adjDistance adjSub2,
         fromJust $ adjPOS adjSub3,  
         S.Word "") --adjWord adjSub3) 

type AdjunctionEvent = (GWord, -- The childword adjoining 
                        Maybe NonTerm, -- the nonterm of the top of the child 
                        AdjunctionType -- is this a sister or regular adjunction 
                       )


data TAGTrainingCounts = 
    TAGTrainingCounts  {
      spineCounts :: CondObserved Spine GWord, -- pick the spine 
      leftAdjCounts  :: CondObserved (Maybe AdjunctionEvent) AdjunctionContext,
      rightAdjCounts :: CondObserved (Maybe AdjunctionEvent) AdjunctionContext 
    } 
    deriving Eq

instance Binary TAGTrainingCounts where 
    put ttc = (put $ spineCounts ttc) >> 
              (put $ leftAdjCounts ttc) >> 
              (put $ rightAdjCounts ttc)
    get = return TAGTrainingCounts `ap` get `ap` get `ap` get 

newtype TAGProbs = TAGProbs
    (CondDistribution Spine GWord, 
     CondDistribution (GWord, Maybe NonTerm) AdjunctionContext) 


instance (Pretty d, Monoid d ) => Pretty (Derivation d) where
    pPrint = pPrint . fromDerivation 
    
instance Pretty TAGTrainingCounts where 
    pPrint (TAGTrainingCounts a b c) = 
        vcat  $ map text ["Spine: " ++ show a, 
               "Left:  " ++ show b, 
               "Right: " ++ show c]


instance Show TAGTrainingCounts where 
    show (TAGTrainingCounts a b c) = 
        intercalate "\n"  ["Spine: " ++ show a, 
                           "Left: " ++ show b, 
                           "Right: " ++ show c]

type TAGCountSemi = Derivation TAGTrainingCounts 

instance Monoid TAGTrainingCounts  where 
    mempty = TAGTrainingCounts mempty mempty mempty
    mappend (TAGTrainingCounts a b c) (TAGTrainingCounts a' b' c') = 
        TAGTrainingCounts (mappend a a') (mappend b b') (mappend c c')

initSemiCounts (word, spine) = 
    mkDerivation $ mempty {spineCounts = singletonObservation spine word}

mkTagWords initSemi words = 
    mkSentence $ zip (map initSemi words) words



instance Arbitrary Spine where
    arbitrary = Spine `liftM` listOf1 arbitrary

instance WordSym TAGWord where
    root = (root, Spine [NonTerm "ROOT"])

instance Show Spine where 
    show (Spine nts) = intercalate "+" $ ["*"] ++ map show nts

data TAGSentence semi = 
    TAGSentence (Sentence semi TAGWord) (Dependency AdjunctionInfo)
                      deriving (Show)
instance (Arbitrary (TAGSentence TAGCountSemi)) where 
    arbitrary = do
      sent <- listOf1 arbitrary
      let tsent = mkTagWords initSemiCounts sent 
      dep <- arbDepMap (sentenceLength tsent) (pickAdjInd tsent)
      return $ TAGSentence tsent dep
          where pickAdjInd sent i = do 
                  let (_, Spine sp) = getWord sent i
                  adjPos <- choose (0,length sp -1)
                  return (adjPos, Sister)
--estimateTAGProb (TAGTrainingCounts (spineCounts, adjCounts)) = 
--    TAGProbs (
--             estimateConditional estimateWittenBell spineCounts,
--             estimateConditional estimateWittenBell adjCounts 
--            )

type AdjunctionInfo = (Int, -- adjunction position
                       AdjunctionType) -- sister adjunction? 

--newtype TAGDerivation = TAGDerivation (M.Map (Word,Spine) Int,
--                                       Dependency AdjunctionInfo) 

--initSemi (TAGProbs (init ,_)) (word, spine) =
--    mkViterbi $ 
--    Weighted (Prob $ prob (cond init word) spine) $ 
--              mkDerivation $ TAGDerivation (M.singleton (word,spine), 
--                                           Dependency M.empty) 


zipMap fn ls = zip ls $ map fn ls  
mapZip fn ls = zip (map fn ls) ls  


-- | Estimate the counts of a supervised TAG sentence 
--   from data. 
-- directCounts (TAGSentence sent dep) =
--     TAGTrainingCounts spineCounts adjCounts adjCounts 
--         where
--           adjCounts = mconcat $
--               map (\(i, DEdge head info) -> 
--                        mkAdjunction (getWord sent head) 
--                                     (getWord sent i) 
--                                     (head, DEdge i info)) $ 
--               zipMap (getHead dep) [1..n]
--           spineCounts = mconcat $ 
--               map (\(word, spine) -> 
--                      singletonObservation spine word) $ 
--               map (getWord sent) [1..n]
--           n = sentenceLength sent

type TAGFSM semi = GraphWFSM Int (Maybe TAGWord) semi
type GetSemi word edge semi =  (Int,word) -> Maybe (Int,word) -> edge -> semi


tagSentenceFSMs (TAGSentence sent dep)  = 
    map (\(i, e) -> makeTagFSM (getWord sent) i e) $
    flattenDep dep

-- | Assert- left is reverse order, right is in order
makeTagFSM getWordInfo headi (left, right)  = 
    (makeDirFSM True left, makeDirFSM False right)
    where  makeDirFSM side expObs= 
                      expectFSMTag $
                                    [((getWordInfo.fst) `liftM` childInd,
                                      mkCountSemi side $ mkAdjunction (headi,word) 
                                             (fmap (\(ind,sis)-> (ind, getWordInfo ind, sis)) childInd) 
                                             pos)
                                     | (pos, (adjs))  <- aligned,
                                       childInd <- adjs
                                    ]
                      where aligned = alignWithSpine spine expObs 
           word@(_,spine) = getWordInfo headi

-- | Takes a spine and an ordered list of adjunctions, 
--   returns the list of adjunctions with epsilons inserted 
alignWithSpine (Spine spine) adjs =
    [(pos, getAdj nt pos ++ [Nothing]) | 
     (nt, pos) <- zip spine [0..]] 
    where 
      indexedAdj = M.fromListWith (flip (++)) $ 
                   map (\(DEdge to (pos, sister)) -> ((pos), [(to,sister)]) ) adjs
      getAdj nt ind  = 
          case M.lookup ind indexedAdj of
            Nothing -> [] 
            Just adjs -> map Just adjs  



testAlign = TestCase $  
            mapM_ (\ ((sp, adj), ans) -> assertEqual "alignment" ans (alignWithSpine sp adj)) test
    where  test = [((Spine [NonTerm "A", NonTerm "B"], [DEdge 10 (1, Regular) ]), 
                    [(0,[Nothing]),(1, [Just (10, Regular),Nothing])]),

                   ((Spine [NonTerm "A", NonTerm "B"], [DEdge 10 (1, Regular), DEdge 12 (1, Regular), DEdge 15 (1, Sister)  ]), 
                    [(0,[Nothing]),(1,[Just (10, Regular) ,Just (12, Regular), Just (15, Sister), Nothing])]),
                  ((Spine [NonTerm "A"], [DEdge 4 (0, Sister), DEdge 3 (0, Sister), DEdge 2 (0, Sister)]), 
                    [(0,[Just (4, Sister), Just (3, Sister) , Just (2, Sister), Nothing])])]
                   

                


mkCountSemi side adj  =
    mkDerivation $ counts
        where 
          counts = 
              if side then 
                  mempty {leftAdjCounts  = adj} 
              else 
                  mempty {rightAdjCounts =  adj} 

mkAdjunction (headInd, ((headWord, headPOS), headSpine))  
             child
             pos =
    singletonObservation 
    (fmap (\(_,(childWord, childSpine), sister) ->  
               (childWord, -- r 
                top childSpine, -- R,  
                sister -- sister
               )) 
     child) 
    (getNonTerm pos headSpine, -- H
     maybe True (\(cind,_, _)->abs (headInd -cind) == 1) child, -- delta
     headPOS, --t
     headWord)

expectFSMTag transitions = 
    makeWFSM 0 n (Just Nothing) ((n, []):
           [ (i-1, [(trans, (i, semi))])
            | ((trans,semi), i) <- zip transitions [1..]])  
        where n = length transitions
    
-- prop_directCheck tagsent = --trace ((show finalSemi) ++ "\n\n" ++ (show $ directCounts tagsent))  $  
--     (directCounts tagsent == fromDerivation finalSemi)
--     where types = tagsent:: (TAGSentence (Derivation TAGTrainingCounts)) 
--           (TAGSentence sent _) = tagsent 
--           fsms = tagSentenceFSMs tagsent mkTagDepDerivation
--           getFSM i word = fsms !! (i-1)
--           (Just finalSemi, _) = eisnerParse getFSM sent
