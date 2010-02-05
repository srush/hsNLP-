{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, FlexibleInstances, FlexibleContexts  #-}  
module NLP.Model.Dependency.Parse where 

import NLP.Model.ParseState
import NLP.Model.Dependency.Model
import NLP.Model.Dependency.Wrap
import Helpers.Common
import NLP.Probability.Chain
import NLP.Model.CreateableSemi
import NLP.Model.Distance
import NLP.FSM
import NLP.Semiring
import NLP.Language.SimpleLanguage
import Debug.Trace

instance ParseModel FirstOrderDep where 
    type Req1 FirstOrderDep = (DWord, ALabel, VerbDistance)
    type MyWord FirstOrderDep = DWord
    mkEventAndContext adjstate (dword, label, vdis) = (fullEvent, fullContext)
        where 
          fullContext = DepFullContext {
                                    adjSide     = side,
                                    crossesVerb = vdis, 
                                    delta       = curDelta, 
                                    parentPOS   = getPOS headWord,
                                    parentWord  = getLex headWord,
                                    parentInd   = ind 
                                  }
          fullEvent   = DepFullEvent {
                                    childWord   = getLex dword,
                                    childPOS    = getPOS dword,
                                    childLabel  = label,
                                    childInd    = dwInd dword
                                  }
          AdjState {opts  = opts, 
                    side  = side, 
                    curPos= pos,
                    word  = headWord,
                    curDelta  = curDelta,
                    lastInNPB = npblast,
                    ind = ind
                    } = adjstate

tryParent state child split = do 
  label <- labelList state --trace ("trying parent" ++ show state ++ " " ++show child ++ " " ++ show split  ) $ labelList state
  s <- findSemi state (child, label, mkDistance state split)
  return $ 
         (state{curDelta = newDelta,
                  isAfterComma = prevComma oldDelta
                 }, s)
      where 
        oldDelta = curDelta state
        newDelta = if (predComma state) child then withPrevComma oldDelta
                   else resetDelta


instance Show (AdjState DWord a b) where 
    show s = (show $ word s) ++ (show $ side s)
       
instance (CreateableSemi semi, Semiring semi, Model semi ~ FirstOrderDep) => 
    WFSM (AdjState DWord FirstOrderDep semi) where 
    type State (AdjState DWord FirstOrderDep semi) = AdjState DWord FirstOrderDep semi 
    initialState init = [(init, one)] 
    
instance (CreateableSemi semi, Semiring semi, Model semi ~ FirstOrderDep) => 
    FSMState (AdjState DWord FirstOrderDep semi) where 
    type FSMSymbol (AdjState DWord FirstOrderDep semi) = DWord
    type FSMSemiring (AdjState DWord FirstOrderDep semi) = semi 
    next = tryParent
    finish _ _ = Just one -- TODO- check this
    isFinal = const True  
