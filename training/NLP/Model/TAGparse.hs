{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, FlexibleInstances, FlexibleContexts  #-}  
module NLP.Model.TAGparse where 

--{{{  Imports 
import Helpers.Common hiding (Derivation)
import NLP.FSM
import NLP.ChartParse
-- import NLP.ChartParse.Eisner
import NLP.Grammar.Dependency
import NLP.WordLattice
import qualified Data.Map as M
import qualified Data.Set as S
import NLP.Grammar.TAG hiding (adjType)
import NLP.Grammar.NonTerm
import NLP.Grammar.Spine
import NLP.Language
import NLP.Semiring
--import ExtraParams
import NLP.Model.CreateableSemi
import NLP.Model.Distance
import NLP.Model.Adjunction
import NLP.Model.Chain
import Debug.Trace
import NLP.Model.TAGWrap
--}}}

-- | Sentence level run-time options
data ParseOpts m semi l  = ParseOpts {
      useCommaPruning :: Bool,
      distanceCache :: DisCache,
      model :: ProbModel m semi l 
}

data AdjState m semi l = 
    AdjState { 
      opts :: ParseOpts m semi l,
      
      word :: TWord l,
      curPos :: !Int, 
      side :: !AdjunctionSide,
      curDelta :: !Delta,
      isAfterComma :: !Bool,
      lastInNPB :: Maybe (GWord l) 
      -- stateParents :: M.Map (Int, VerbDistance, Delta) AdjunctionParent 
} 

initState :: (Language l) =>
           ParseOpts model semi l ->
           AdjunctionSide ->
           TWord l ->
           AdjState model semi l 
initState parseOpts side tagword = AdjState {
                                     opts = parseOpts,

                                     word = tagword,
                                     side = side,                                
                                     curPos = 0,
                                     curDelta = startDelta,
                                     isAfterComma = False,

                                     lastInNPB = Nothing
                   }
    where 
      last = lastOfSpine $ twSpine tagword

--stateNT :: AdjState l -> NonTerm l 
stateNT adjstate = getNonTerm (curPos adjstate) $ twSpine (word adjstate)
    
stateSpine = twSpine . word 

topPos = lastOfSpine . twSpine . word 
isComplete state = curPos state >= topPos state 

--{{{  AdjState Classes 
{-# INLINE expandAdjState #-}
expandAdjState as =  (curPos as, curDelta as, isAfterComma as, lastInNPB as, side as)

instance (Language l) => Show (AdjState a b l) where 
    show s = (show $ curPos s) ++ (show $ ((lastOfSpine $ twSpine $ word s) - 1) ) ++ (show $ side s)

instance (Language l) => Eq (AdjState a b l) where 
    (==) = (==) `on` expandAdjState

instance (Language l) => Ord (AdjState a b l) where
    {-# INLINE compare #-}
    compare = compare `on` expandAdjState
--}}}

block = guard . not

-- | This function is given an Adjunction state, a child to adjoin and the 
--   split point between them in the sentence. It tries to adjoin that child into 
--   each spot of the tree. 
tryAdjunction state child split = do
 
  -- should we prune this adjunction?
  block $ shouldPrune state split False

  -- enumerate all states up the tree with empty adjunctions 
  (state', semi) <- tryEmpties split state

  -- are there spots left to adjoin?
  block $  isComplete state'

  -- enumerate all adjunction types
  atype <- [Sister, Regular]

  -- do each possible adjunction
  doOneAdj semi state' child atype $ mkDistance state' split


-- | This helper function is given an Adjunction state and a split point.  
--   It returns each AdjState up the tree assuming that the End symbol is chosen.
tryEmpties split state = tryEmpties' (state,one)
    where 
      tryEmpties' (adjstate, semi) = 
          (adjstate, semi): do

            -- Are we already complete?
            block $ isComplete adjstate

            -- Tells us if this 
            semi' <- findSemi adjstate Nothing Sister (mkDistance adjstate split)

            -- Should we prune this node?
            block $ shouldPrune adjstate split True

            -- pop and continue
            tryEmpties'  (adjstate {curPos = curPos adjstate + 1, 
                                             curDelta = startDelta,
                                             isAfterComma = False,
                                             lastInNPB = Nothing}, 
                                       semi `times` semi')

-- | This function is called only to finish a tree. 
-- That is add end symbols to each node until it is complete.
tryFinish state split =
    if isComplete endstate then semi else mempty
    where (endstate, semi) = last $ tryEmpties split state

-- | Helper function, perform an adjunction
doOneAdj baseSemi adjstate child atype dis = do 
    s <- findSemi adjstate child atype dis
    return  (adjstate{curDelta = newDelta,
                      isAfterComma = prevComma oldDelta,
                      lastInNPB = if npbMode then 
                                      if twIsComma child' then
                                          lastInNPB adjstate
                                      else Just $ twWord $ fromJustNote "read" child 
                                  else Nothing
                     }, 
             baseSemi `times` s)
          where   
            npbMode = isWrapNPB $ stateNT adjstate
            child' = fromJustNote "real" child
            oldDelta = curDelta adjstate
            newDelta = if twIsComma child' then oldDelta {prevComma = True} 
                       else resetDelta


findSemi :: (Language l, CreateableSemi semi) => 
            AdjState (Collins l) semi l -> Maybe (TWord l) -> 
            AdjunctionType -> VerbDistance -> [semi l]
findSemi adjstate child atype vdis = do 
    guard $ (validity $ model opts) headWord child pos atype
    return $ mkSemi p headWord child pos atype
        where
          fullContext = AdjunctionFullContext {
                          parentNT = stateNT adjstate,
                          headNT   = (fromJustDef (fromPOS $ getPOS $ twWord headWord) $ 
                                      lookupNonTerm (pos-1) $ twSpine $ headWord) -- Fix this 
                                     ,
                          adjSide  = side,
                          delta = curDelta,
                          crossesVerb = vdis', 
                          parentPOS = getPOS $ parentGWord, 
                          parentWord = getLex $ parentGWord,
                          parentInd  = twInd headWord
                                     }
           
              where          -- NPB Trick 
                (parentGWord, vdis') = 
                    case lastInNPB adjstate of 
                      (Just w) -> (w, VerbDistance False)
                      Nothing -> (twWord headWord, vdis) 

          fullEvent  = case child of
                         Nothing -> emptyAdjunction
                         Just child' -> AdjunctionFullEvent {
                                         childWord = Just $ getLex $ twWord child',
                                         childPOS  = Just $ getPOS $ twWord child',
                                         childSpine= Just $ twSpine child',
                                         childInd  = Just $ twInd child',
                                         adjType   = Just atype
                                       }
                                       
          p = (probs $ model opts) (chainRule fullEvent fullContext)  -- TODO
          AdjState {opts  = opts, 
                    side  = side, 
                    curPos= pos,
                    word  = headWord,
                    curDelta  = curDelta,
                    lastInNPB = npblast
                    } = adjstate

-- | Implements comma pruning 
commaPruning state split = 
    (side state == ARight) &&
    (isAfterComma state) && 
    (not $ afterConj)
        where afterConj = snd $ (distanceCache $ opts state) (twInd $ word state, split) 

shouldPrune adjstate split isTryingEmpty = 
    (isTryingEmpty && (prevComma $ curDelta adjstate)) || 
    ((useCommaPruning $ opts adjstate)  && commaPruning adjstate split)

mkDistance adjstate split = VerbDistance verb
    where (verb, _) = (distanceCache $ opts adjstate) (twInd $ word adjstate, split)

type Validity l = TWord l -> (Maybe (TWord l)) -> Int -> AdjunctionType -> Bool
allValid _ _ _ _ = True

data ProbModel m s l = ProbModel {
      probs :: (Pairs m -> Counter s),
--      extra :: FlipProbs,
      validity :: Validity l 
    }

instance (Language l, CreateableSemi semi, Semiring (semi l)) => 
    WFSM (AdjState (Collins l) semi l) where 
    type State (AdjState (Collins l) semi l) = AdjState (Collins l) semi l
    initialState init =  [(init, one)] 
    
instance (Language l, CreateableSemi semi, Semiring (semi l)) => 
    FSMState (AdjState (Collins l) semi l) where 
    type FSMSymbol (AdjState (Collins l) semi l) = Maybe (TWord l)
    type FSMSemiring (AdjState (Collins l) semi l) = semi l
    next = tryAdjunction
    finish = tryFinish 
    isFinal = const True  


-- if (lastOfSpine $ twSpine $ stateHead init) == 0 then [(init, one)]
--                         else
--                             [(init{stateSide = ALeft }, 
--                           mkSemiSmall (probFlip flipprobs ALeft $ flip)),
--                          (init{stateSide = ARight},
--                           mkSemiSmall (probFlip flipprobs ARight $ flip)
--                           )]
--         where flipprobs = extra $ stateModel init
--               flip = mkFlip (stateHead init) (stateSide init)

-- tryAdjunction :: (Semiring semi, FSMState (AdjState a) ) => 
--                (AdjState a -> Int -> AdjunctionAction (TWord) -> AdjunctionType -> Distance -> Maybe semi) -> 
--                (AdjState a) ->  
--                (Maybe (TWord)) ->
--                Int -> --huge hack!
--                [(AdjState a, semi)] 

--     else if stateNT adjstate == rootNT then
--              if stateSide adjstate == ARight then
--                  []
--              else
--                  case findSemi adjstate (DoAdj $ fromJustNote "its good" child) Sister (mkDistance adjstate split) of 
--                    Nothing -> []
--                    Just s -> 
--                        [(adjstate {statePos = (statePos adjstate) + 1} , s)]
