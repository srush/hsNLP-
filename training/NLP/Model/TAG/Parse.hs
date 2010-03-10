{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, FlexibleInstances, FlexibleContexts  #-}  
module NLP.Model.TAG.Parse where 

--{{{  Imports 
import Helpers.Common hiding (Derivation)
import NLP.FSM
import NLP.ChartParse
-- import NLP.ChartParse.Eisner
import NLP.Grammar.Dependency

import qualified Data.Map as M
import qualified Data.Set as S
import NLP.Grammar.TAG hiding (adjType)
import NLP.Grammar.Spine
import NLP.Language.SimpleLanguage
import Data.Semiring
import NLP.ParseMonad
--import ExtraParams
import NLP.Model.CreateableSemi
import NLP.Model.Distance
import NLP.Model.TAG.Adjunction
import NLP.Probability.Chain
import Debug.Trace
import NLP.Model.TAG.Wrap
import NLP.Model.ParseState
--}}}

--stateNT :: AdjState l -> NonTerm l 
stateNT adjstate = getNonTerm (curPos adjstate) $ twSpine (word adjstate)
    
stateSpine = twSpine . word 

topPos = lastOfSpine . twSpine . word 
isComplete state = curPos state >= topPos state 

--{{{  AdjState Classes 

instance Show (AdjState TWord a b) where 
    show s = (show $ curPos s) ++ (show $ ((lastOfSpine $ twSpine $ word s) - 1) ) ++ (show $ side s)

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
  doOneAdj semi state' (child, atype, mkDistance state' split, split)


-- | This helper function is given an Adjunction state and a split point.  
--   It returns each AdjState up the tree assuming that the End symbol is chosen.
tryEmpties split state = tryEmpties' (state,one)
    where 
      tryEmpties' (adjstate, semi) = 
          (adjstate, semi): do

            -- Are we already complete?
            block $ isComplete adjstate

            -- Tells us if this 
            semi' <- findSemi adjstate (Nothing, Sister, mkDistance adjstate split, split)

            -- Should we prune this node?
            block $ shouldPrune adjstate split True

            -- pop and continue
            tryEmpties'  (cacheState $ adjstate {curPos = curPos adjstate + 1, 
                                             curDelta = startDelta,
                                             isAfterComma = False,
                                             lastInNPB = Nothing,
                                             hasBeenRegular = False}, 
                                       semi `times` semi')

-- | This function is called only to finish a tree. 
-- That is add end symbols to each node until it is complete.
tryFinish state split =
    if isComplete endstate then Just semi else Nothing
    where (endstate, semi) = last $ tryEmpties split state

-- | Helper function, perform an adjunction
doOneAdj baseSemi adjstate (child, atype, dis, split) = do 
    s <- findSemi adjstate (child, atype, dis, split)
    return  (cacheState $ adjstate{curDelta = newDelta,
                                   
                     -- Bikel - Another subtlety is that the comma constraint should effectively 
                     -- not be employed when pursuing theories of an NPB subtree

                      isAfterComma = prevComma oldDelta && not npbMode,

                      lastInNPB = if npbMode then 
                                      if predComma adjstate child' then
                                          lastInNPB adjstate
                                      else Just $ twWord $ fromJustNote "read" child 
                                  else Nothing,
                      hasBeenRegular = atype == Regular || hasBeenRegular adjstate,
                      curPos = (curPos adjstate) + (if isRoot then 1 else 0)
                     }, 
             baseSemi `times` s)
          where   
            npbMode = (predNPB adjstate)  $ stateNT adjstate
            isRoot = (predRoot adjstate)  $ stateNT adjstate
            child' = fromJustNote "real" child
            oldDelta = curDelta adjstate
            newDelta = if (predComma adjstate) child' then withPrevComma oldDelta
                       else resetDelta


instance ParseModel Collins where 
    type Req1 Collins = (Maybe (TWord), AdjunctionType, VerbDistance, Int)
    type MyWord Collins = TWord
    mkEventAndContext = mkEventAndContextTAG

mkEventAndContextTAG adjstate (child, atype, vdis, split) = (fullEvent, fullContext)
    where  
          fullContext = AdjunctionFullContext {
                          parentNT = stateNT adjstate,
                          headNT   = -- (fromJustDef (fromPOS $ getPOS $ twWord headWord) $ 
                                      lookupNonTerm (pos-1) $ twSpine $ headWord -- Fix this 
                                     ,
                          adjSide  = side,
                          delta = curDelta,
                          crossesVerb = vdis', 
                          parentPOS = getPOS $ parentGWord, 
                          parentWord = getLex $ parentGWord,
                          parentInd  = twInd headWord,
                          spinePos   = pos,
                          parentTWord = headWord,
                          prevRegular = atype == Regular || beenReg,
                          inNPB      = npbMode,
                          splitPoint = split
                        }
           
              where          -- NPB Trick 

--{{{  Bikel explanation
-- Another crucial point
-- about the vi predicate is that it does not include verbs that appear within base NPs. Put
-- another way, in order to emulate Collins' model, we need to amend the dnition of cv
-- by stipulating that cv(NPB) = False 
--}}}
                (parentGWord, vdis') = 
                    case lastInNPB adjstate of 
                      (Just w) -> (w, VerbDistance False) -- TODO- check this 
                      Nothing -> (twWord headWord, vdis) 


          fullEvent  = case child of
                         Nothing -> emptyAdjunction
                         Just child' -> AdjunctionFullEvent {
                                         childWord = Just $ getLex $ twWord child',
                                         atomChildSpine = Just $ twAtomSpine child',
                                         childPOS  = Just $ getPOS $ twWord child',
                                         childSpine= Just $ twSpine child',
                                         childInd  = Just $ twInd child',
                                         adjType   = Just atype,
                                         childTWord = Just child'

                                       }
          AdjState {opts  = opts, 
                    side  = side, 
                    curPos= pos,
                    word  = headWord,
                    curDelta  = curDelta,
                    lastInNPB = npblast,
                    hasBeenRegular = beenReg
                    } = adjstate
          npbMode = (predNPB adjstate)  $ stateNT adjstate

instance (CreateableSemi semi, Semiring semi, Model semi ~ Collins) => 
    WFSM (AdjState TWord Collins semi) where 
    type State (AdjState TWord Collins semi) = AdjState TWord Collins semi 
    initialState init =  [(init, one)] 
    
instance (CreateableSemi semi, Semiring semi, Model semi ~ Collins) => 
    FSMState (AdjState TWord Collins semi) where 
    type FSMSymbol (AdjState TWord Collins semi ) = Maybe TWord
    type FSMSemiring (AdjState TWord Collins semi ) = semi 
    next = tryAdjunction
    finish = tryFinish 
    isFinal = const True  


