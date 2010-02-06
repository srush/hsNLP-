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
      probs :: (Pairs m -> Counter s),
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
initState parseOpts  labels side = do
  predComma<- isComma
  predVerb <- isVerb
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


-- | Implements comma pruning 
commaPruning state split = 
    (side state == ARight) &&
    (isAfterComma state) && 
    (not $ afterConj)
        where afterConj = snd $ (distanceCache $ opts state) (ind state, split) 

shouldPrune adjstate split isTryingEmpty = 
--    (isTryingEmpty && (prevComma $ curDelta adjstate)) || 
      ((useCommaPruning $ opts adjstate)  && commaPruning adjstate split)

mkDistance adjstate split = VerbDistance verb
    where (verb, _) = (distanceCache $ opts adjstate) (ind adjstate, split)

class (JointModel a) => ParseModel a where 
    type Req1 a 
    type MyWord a 
    mkEventAndContext :: AdjState (MyWord a) a semi -> Req1 a -> (FullEvent a, FullContext a)  


findSemi :: (ParseModel model, CreateableSemi semi, Model semi ~ model ) => 
            AdjState (MyWord model) model semi -> Req1 model -> [semi]
findSemi adjstate extra = do 
    guard $ (validity $ model opt) fullEvent fullContext
    return $ mkSemi p fullEvent fullContext
        where                          
          (fullEvent, fullContext) = mkEventAndContext adjstate extra 
          p = (probs $ model opt) (chainRule fullEvent fullContext)  -- TODO
          opt = opts adjstate
