{-# LANGUAGE TypeFamilies #-}
module NLP.Model.ParseState where 

--{{{  Imports
import Debug.Trace
import NLP.Language.SimpleLanguage
import NLP.WordLattice

import qualified Data.Map as M
import qualified Data.Set as S
import NLP.Model.Distance
import NLP.ParseMonad
import NLP.Grammar.Dependency
import NLP.Model.CreateableSemi
import Helpers.Common
import NLP.Probability.Chain
import Helpers.MkEnum
import Helpers.Test
--}}}

-- | General state handler for dependency like parsers 

-- | Sentence level run-time options
data ParseOpts m semi  = ParseOpts {
      useCommaPruning :: Bool,
      distanceCache :: DisCache,
      model :: ProbModel m semi  
}

--type Validity = TWord -> Maybe TWord -> Int -> AdjunctionType -> Bool
type Validity m = FullEvent m -> FullContext m -> Bool
allValid _ _ = True

data ProbModel m s = ProbModel {
      probs :: Counter s,
--      extra :: FlipProbs,
      validity :: Validity m
    }



data AdjState w m semi = 
    AdjState { 
      opts :: ParseOpts m semi,      
      word :: w,
      ind  :: !Int,
      curPos :: !Int, 
      side :: !AdjunctionSide,
      curDelta :: !Delta,
      isAfterComma :: !Bool,
      lastInNPB :: Maybe GWord,
      hasBeenRegular :: Bool,
      predComma :: w -> Bool,
      predVerb :: w -> Bool,
      predNPB :: ANonTerm -> Bool,
      predRoot :: ANonTerm -> Bool,
      labelList :: [ALabel],
      enumId :: Int
    } 

--expandAdjState as = (curPos as, curDelta as, isAfterComma as, lastInNPB as, hasBeenRegular as, side as)

-- OPTIMIZATION
expandAdjState as = (enumId as, lastInNPB as)
cacheEnum as = as{ enumId = combineEnum [(fromEnum $  curPos as, 5), (fromEnum $ curDelta as, 10), (fromEnum $ isAfterComma as,5), 
                                         (fromEnum $ side as,5), (fromEnum $ hasBeenRegular as, 5)] }
 
cacheState = cacheEnum

initState :: (WordSymbol w, Show w) => 
           ParseOpts model semi  ->
           [ALabel] ->
           AdjunctionSide ->
           ParseMonad (Int -> w ->
                       (AdjState w model semi))  
initState parseOpts labels side = do
  predComma<- isComma
  predVerb <- isVerb
  predNPB  <- isNPB
  predRoot  <- isRoot
  return $ (\ind tagword -> 
                cacheState $ AdjState {
               opts = parseOpts,
               word = tagword,
               ind = ind,
               side = side,                                
               curPos = 0,
               curDelta = startDelta,
               isAfterComma = False,
               labelList = labels,
               lastInNPB = Nothing,
               hasBeenRegular = False,
               predComma = predComma. getPOS,
               predVerb = predVerb.getPOS,
               predNPB = predNPB,
               predRoot = predRoot,
               enumId = undefined
                   }
           )

{-# INLINE expandAdjState #-}
--expandAdjState as = (curPos as, curDelta as, isAfterComma as, lastInNPB as, side as)

instance  (Eq w) => Eq (AdjState w a b ) where 
    (==) = (==) `on` expandAdjState

instance  (Ord w) => Ord (AdjState w a b) where
    {-# INLINE compare #-}
    compare = compare `on` expandAdjState

--{{{  Comma Pruning explanation

-- The second form of pruning employed is using a comma constraint. Collins observed
-- that in the Penn Treebank data, 96% of the time, when a constituent contained a
-- comma, the word immediately following the end of the constituent's span was either a
-- comma or the end of the sentence. So, for speed reasons, the decoder rejects all theories
-- that would generate constituents that violate this comma constraint. There is a subtlety
-- to Collins' implementation of this form of pruning, however. Commas are quite
-- common within parenthetical phrases. Accordingly, if a comma in an input sentence
-- occurs after an open parenthesis and before a closing parenthesis or the end of the sentence,
-- it is not considered a comma for the purposes of the comma constraint. Another
-- subtlety is that the comma constraint should effectively not be employed when pursuing
-- theories of an NPB subtree. As it turns out, using the comma constraint also affects
-- accuracy, as shown in §7.1.

--}}}

-- | Implements comma pruning 
commaPruning state split = 
    (side state == ARight) &&
    (isAfterComma state) && 
    (not $ afterConj)
        where afterConj = endsWithComma $ (distanceCache $ opts state) (ind state, split) 

shouldPrune adjstate split isTryingEmpty = 
--    (isTryingEmpty && (prevComma $ curDelta adjstate)) || 
    ((useCommaPruning $ opts adjstate)  && commaPruning adjstate split)

mkDistance adjstate split = verb
    where verb = containsVerb $ (distanceCache $ opts adjstate) (ind adjstate, split)

class (JointModel a) => ParseModel a where 
    type Req1 a 
    type MyWord a 
    mkEventAndContext :: AdjState (MyWord a) a semi -> Req1 a -> (FullEvent a, FullContext a)  


findSemi :: (ParseModel model, CreateableSemi semi, Model semi ~ model ) => 
            AdjState (MyWord model) model semi -> Req1 model -> [semi]
findSemi adjstate extra = do 
    guard $ (validity $ model opt) fullEvent fullContext
    return $ mkSemi (probs $ model opt) fullEvent fullContext
        where                          
          (fullEvent, fullContext) = mkEventAndContext adjstate extra 
          opt = opts adjstate


--{{{  TESTS

testParseState = testGroup "ParseState props" [
           
        ]



--}}}
