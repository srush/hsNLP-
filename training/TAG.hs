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
import Data.Function (on)
import Control.Monad (ap)
import Data.Maybe (isNothing)
import Safe (fromJustNote, atNote)

import Data.Monoid
import NLP.Semiring
import NLP.Semiring.Derivation
import NLP.Semiring.Prob
import NLP.Semiring.Viterbi
import NLP.Semiring.ViterbiNBestDerivation

import NLP.FSM.Simple
import NLP.FSM
import NLP.Probability.ConditionalDistribution
import NLP.Probability.Distribution
import NLP.ChartParse
import NLP.ChartParse.Eisner
import Debug.Trace
import Debug.Trace.Helpers
import Data.Generics
import Control.Exception

import Text.PrettyPrint.HughesPJClass

import Test.HUnit hiding (State, Node, assert)

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

top (Spine []) = Nothing
top (Spine nts) = Just $ last nts

lastOfSpine (Spine nts) = length nts

getNonTerm i (Spine nts) = atNote "getNonTerm" nts  i

lookupNonTerm i (Spine nts) =
    if i >= length nts then Nothing
    else Just $ nts !! i

instance Show Spine where 
    show (Spine nts) = intercalate "+" $ ["*"] ++ map show nts


data TAGWord = TAGWord {
      twInd   :: Int,
      twWord  :: GWord,
      twSpine :: Spine}
               deriving (Eq, Ord, Show)

instance Context GWord where 
    type Sub (GWord) = String
    decompose (S.Word word, POS pos) = [pos, word] 
    compose [pos, word] = (S.Word word, POS pos)


data Distance = Distance { isAdjacent :: Bool,
                           spansVerb  :: Bool} 
    deriving (Show, Eq, Ord)
$( derive makeBinary ''Distance )

type AdjunctionContext = 
    (NonTerm, -- The nonterminal adjoining into 
     Distance, -- The distance between the lexical items 
     POS, -- , -- pos of the word head spine
     S.Word) -- the word of the head spine

-- this is lame, but need it to make subcontext type safe
data AdjunctionSub = AdjunctionSub {
      adjNonTerm :: Maybe NonTerm,
      adjDistance:: Maybe Distance,
      adjPOS :: Maybe POS,
      adjWord :: Maybe S.Word
       }
                     deriving (Eq, Ord)
$( derive makeBinary ''AdjunctionSub )

instance Show AdjunctionSub where 
    show a = concatMap (\f-> f a) 
             [maybeShow . adjNonTerm, 
              maybeShow. adjDistance,
              maybeShow. adjPOS,
              maybeShow. adjWord 
              ]  
        where maybeShow Nothing = ""
              maybeShow (Just m) = show m ++ " " 

adjSubDef = AdjunctionSub Nothing Nothing Nothing Nothing

instance Context AdjunctionContext where
    type Sub (AdjunctionContext) = AdjunctionSub
    decompose (nonterm, dis, pos, word) = 
        [adjSubDef {adjNonTerm =Just nonterm,
                    adjDistance = Just dis},
         adjSubDef {adjPOS = Just pos},
         adjSubDef {adjWord = Just word}]
        --[adjSubDef {adjNonTerm =Just nonterm},
         --adjSubDef {adjDistance = Just dis},
         --adjSubDef {adjPOS = Just pos}] -- simpler decomp
    compose [adjSub1,adjSub2,adjSub3] =     
        (fromJustNote "" $ adjNonTerm adjSub1, 
         fromJustNote "" $ adjDistance adjSub1,
         fromJustNote "" $ adjPOS adjSub2,  
         fromJustNote "" $ adjWord adjSub3) 

type AdjunctionEvent = (POS, -- The childword adjoining 
                        Maybe NonTerm, -- the nonterm of the top of the child 
                        AdjunctionType -- is this a sister or regular adjunction 
                       )


data TAGTrainingCounts = 
    TAGTrainingCounts  {
      spineCounts :: CondObserved Spine GWord, -- pick the spine 
      leftAdjCounts  :: AdjunctionObserved,
      rightAdjCounts ::AdjunctionObserved 
    } 
    deriving Eq

type AdjunctionObserved = CondObserved (Maybe AdjunctionEvent) AdjunctionContext

type AdjunctionDist = CondDistribution (Maybe AdjunctionEvent) AdjunctionContext
instance Binary TAGTrainingCounts where 
    put ttc = (put $ spineCounts ttc) >> 
              (put $ leftAdjCounts ttc) >> 
              (put $ rightAdjCounts ttc)
    get = return TAGTrainingCounts `ap` get `ap` get `ap` get 

data TAGProbs = TAGProbs
    { spineProbs :: CondDistribution Spine GWord, 
      leftProbs  :: CondDistribution (Maybe AdjunctionEvent) AdjunctionContext,
      rightProbs  :: CondDistribution (Maybe AdjunctionEvent) AdjunctionContext
    } 


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

initSemiCounts tagWord  = 
    mkDerivation $ mempty {spineCounts = singletonObservation (twSpine tagWord)  (twWord tagWord)}

--initSemiProb (word, spine) = 
--    mkDerivation $ mempty {spineCounts = singletonObservation spine word}


mkTagWords initSemi words = 
    mkSentence $ zip (map initSemi newWords) newWords
               where newWords = map (\(i, (a,b)) -> TAGWord i a b) $ zip [1..] words



instance Arbitrary Spine where
    arbitrary = Spine `liftM` listOf1 arbitrary



instance WordSym TAGWord where
    root ind = TAGWord ind (root ind) (Spine [NonTerm "ROOT"])


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
                  let tagWord = getWord sent i
                  let Spine sp = twSpine tagWord
                  adjPos <- choose (0,length sp -1)
                  return (adjPos, Sister)

estimateTAGProb (TAGTrainingCounts spineCounts leftAdjCounts rightAdjCounts) = 
    TAGProbs (estimateConditional estimateWittenBell spineCounts)
             (estimateConditional estimateWittenBell leftAdjCounts) 
             (estimateConditional estimateWittenBell rightAdjCounts) 
            

type AdjunctionInfo = (Int, -- adjunction position
                       AdjunctionType) -- sister adjunction? 

newtype TAGDerivation = TAGDerivation (M.Map TAGWord Int,
                                       Dependency AdjunctionInfo) 
    deriving (Eq, Monoid, Show)

mkTestTAGWord counts probs (ind, word) = 
    mapZip (initSemiProbs probs) $ map (TAGWord ind word) $ getPosSpines counts word 

getPosSpines counts word =   
    observedInContext word $ spineCounts counts 


initSemiProbs :: TAGProbs -> TAGWord -> ViterbiDerivation TAGDerivation 
initSemiProbs (TAGProbs init _ _) tword@(TAGWord ind word spine) =  
    viterbiHelp (prob (cond init word) spine)
                (M.singleton tword 1, 
                  Dependency M.empty)

viterbiHelp prob td = 
    mkViterbi $ Weighted (Prob prob, mkDerivation $ TAGDerivation td)

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
                                      mkCountSemi side $ mkAdjunction getWordInfo tword
                                             (fmap (getWordInfo.fst) childInd) 
                                             pos (maybe Sister snd childInd) )
                                     | (pos, adjs)  <- aligned,
                                       childInd <- adjs
                                    ]
                      where aligned = alignWithSpine (twSpine tword) expObs 
           tword = getWordInfo headi

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

mkAdjunctionEvent child sister = do
  (TAGWord cins (childWord, childPOS) childSpine) <- child
  return (childPOS, -- r 
          top childSpine, -- R,  
          sister -- sister
         )
   
isVerb tword = take 2 pos == "VB"
    where (_, POS pos) = twWord tword 

between a b = [(min a b) +1  .. (max a b) -1] 


mkAdjunctionContext :: (Int -> TAGWord) -> 
                       TAGWord -> 
                       Maybe TAGWord -> 
                       Int -> AdjunctionContext
mkAdjunctionContext getWord
                    (TAGWord headInd (headWord, headPOS) headSpine)  
                    child
                    pos  =
    (getNonTerm pos headSpine, -- H
     maybe (Distance True False) 
           (\tword -> Distance 
                      (abs (headInd - (twInd tword)) == 1)
                      (any (isVerb . getWord) $ between headInd (twInd tword))
                                     ) child, -- delta
     headPOS, --t
     headWord)

                 
mkAdjunction getWordInfo head child pos sister =
    singletonObservation 
    (mkAdjunctionEvent child sister)
    (mkAdjunctionContext getWordInfo head child pos)

expectFSMTag transitions = 
    makeWFSM 0 n (Just Nothing) ((n, []):
           [ (i-1, [(trans, (i, semi))])
            | ((trans,semi), i) <- zip transitions [1..]])  
        where n = length transitions


data AdjState = 
    AdjState { 
      stateProbs :: !AdjunctionDist,
      stateHead :: !TAGWord,
      statePos :: !Int, 
      stateGetWord :: (Int -> TAGWord),
      stateFinal :: !Bool,
      stateNT :: NonTerm,
      stateNext :: Maybe AdjState 

} 

initAdj getWord probs tagword = 
    fromJustNote "nonblank" $ mkEmpty 0 $ lastOfSpine $ twSpine tagword
    where 
      mkEmpty i last = 
          if i > last then Nothing  
          else Just $ AdjState {
                    stateProbs = probs,
                    stateHead = tagword,
                    statePos = i,
                    stateNT = if last == i then undefined  else getNonTerm i $ twSpine tagword,
                    stateFinal = last == i,
                    stateGetWord = getWord,
                    stateNext = mkEmpty (i+1) last 
                   }

stateSpine adjstate = twSpine $ stateHead adjstate

instance Eq AdjState where 
    (==) = (==) `on` statePos

instance Ord AdjState where 
    compare = compare `on` statePos

instance Show AdjState where 
    show = show . statePos



instance WFSM AdjState where 
    type State AdjState = AdjState 
    initialState init = tryEmpties (init, one)

tryEmpties (adjstate, semi) = (adjstate, semi):
    if isFinal adjstate then [] 
    else
     assert (statePos adjstate < (lastOfSpine $ stateSpine adjstate)) $  
        tryEmpties (fromJustNote "not final" $ stateNext adjstate, semi `times` semi')
        where
          pos = statePos adjstate
          head = stateHead adjstate
          probs = stateProbs adjstate
          getWord = stateGetWord adjstate
          semi' = viterbiHelp  (prob (cond probs $ mkAdjunctionContext getWord head Nothing pos)
                                Nothing)
                               (mempty, 
                                Dependency M.empty)


instance FSMState AdjState where
    type FSMSymbol AdjState = Maybe TAGWord
    type FSMSemiring AdjState = ViterbiDerivation TAGDerivation
    next adjstate child = 
        if isFinal adjstate then [] else
            assert (statePos adjstate < (lastOfSpine $ stateSpine adjstate)) $  
        concatMap tryEmpties $ do
                 atype <- [Sister, Regular]
                 let semi = findSemi nt atype
                  --         trace ("Head: " ++ show head ++ " Child: " ++ show child ++ "prob: " ++ (show (prob (cond probs $ mkAdjunctionContext getWord head child pos ) $ 
                 --             mkAdjunctionEvent child atype))) $ 
                 return $ (adjstate, semi)
                               
        where 
          findSemi nt atype = 
              viterbiHelp 
              (prob (cond probs $ mkAdjunctionContext getWord head child pos ) $ 
                    mkAdjunctionEvent child atype)
              (mempty,  singletonDep (twInd head) (twInd $ fromJustNote "index" child) (pos, atype))
          nt = stateNT adjstate
          pos = statePos adjstate
          head = stateHead adjstate  
          getWord = stateGetWord adjstate
          probs = stateProbs adjstate

    isFinal adjstate = stateFinal adjstate  

-- prop_directCheck tagsent = --trace ((show finalSemi) ++ "\n\n" ++ (show $ directCounts tagsent))  $  
--     (directCounts tagsent == fromDerivation finalSemi)
--     where types = tagsent:: (TAGSentence (Derivation TAGTrainingCounts)) 
--           (TAGSentence sent _) = tagsent 
--           fsms = tagSentenceFSMs tagsent mkTagDepDerivation
--           getFSM i word = fsms !! (i-1)
--           (Just finalSemi, _) = eisnerParse getFSM sent
