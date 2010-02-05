{-# LANGUAGE TypeFamilies, ExistentialQuantification, FlexibleContexts #-}
module NLP.ChartParse.Eisner.Inside where 

--{{{  Imports
import NLP.ChartParse
import Helpers.Common
import Data.List (find)
import NLP.FSM
import NLP.Semiring
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Monoid.Multiplicative (times, one) 
import Control.Exception
import NLP.WordLattice
import Debug.Trace 
--}}}

type EisnerChart fsa = Chart (ESpan fsa) (FSMSemiring (State fsa))
type Semi fsa = FSMSemiring (State fsa)
type Sym fsa = FSMSymbol (State fsa)
type EItem fsa = Item (ESpan fsa) (Semi fsa) 


data Seal a = Sealed | Open a
              deriving (Eq, Ord, Show)

isSealed (Sealed)= True
isSealed _ = False

isStateFinal (Open a) = isFinal a  
isStateFinal (Sealed) = True -- Check this! 


fromSeal (Open a) = a 

 
-- Data structure from p. 12 declarative structure  
data ESpanEnd fsa =
    ESpanEnd {
      hasParent :: !Bool, -- b1 and b2 (does the parent exist in the span, i.e. it's not the head)     
      state :: !(Seal (State fsa)), -- q1 and q2
      word  :: !(Sym fsa),
      split :: !(Maybe Int)
}


--{{{  ESpanEnd Classes

instance (WFSM fsa) => Show (ESpanEnd fsa) where 
    show end = intercalate " " 
               [(show $ hasParent end),
                (show $ state end),
                (show $ word end),
                (show $ split end)] 

expandESpanEnd sp =  (hasParent sp, state sp, word sp, split sp) --   TODO- fix back, 

instance (WFSM fsa) => Eq (ESpanEnd fsa) where 
    (==)  = (==) `on` expandESpanEnd

instance (WFSM fsa) => Ord (ESpanEnd fsa) where 
    compare = compare `on` expandESpanEnd 
--}}}


data ESpan fsa =
    ESpan {
      simple :: !Bool, -- s
      leftEnd :: !(ESpanEnd fsa),
      rightEnd :: !(ESpanEnd fsa)
} deriving (Eq, Ord) 

--{{{  ESpan Classes
instance (WFSM fsa) => Show (ESpan fsa) where
    show span = printf "spL = %s spR = %s s = %s b = %s s = %s wl = %s wr = %s"                 
                (show $ split $ leftEnd span) 
                (show $ split $ rightEnd span) 
                (showBool $ simple span) (showBoolPair $ hasParentPair span) (show (state $ leftEnd span, state $ rightEnd span)) (show $ word $ leftEnd span) (show $ word $ rightEnd span)
        where showBool True = "1"
              showBool False = "0"
              showBoolPair :: (Bool, Bool) -> String
              showBoolPair (a,b) = printf "(%s %s)" (showBool a) (showBool b) 
              

instance (WFSM fsa) => Pretty (ESpan fsa) where
    pPrint span = text $ printf "spL = %s spR = %s wl = %s wr = %s s = %s b = %s sim = %s" 
                (show $ split $ leftEnd span) 
                (show $ split $ rightEnd span) 
                (show $ word $ leftEnd span) 
                (show $ word $ rightEnd span)
                (show (state $ leftEnd span, state $ rightEnd span)) 
                (showBoolPair $ hasParentPair span)
                (showBool $ simple span)  
        where showBool True = "1"
              showBool False = "0"
              showBoolPair :: (Bool, Bool) -> String
              showBoolPair (a,b) = printf "(%s %s)" (showBool a) (showBool b) 
--}}}
                               
hasParentPair span = 
    (hasParent $ leftEnd span , hasParent $ rightEnd span) 

-- Advances an internal WFSM (equivalent in this model to "adjoining" a new
-- dependency. 
advance :: (WFSM fsa) => ESpanEnd fsa  -> Sym fsa -> 
           [(ESpanEnd fsa, Semi fsa)] 
advance headESpan nextWord  = do 
    let split' = fromJustNote "split" $ split headESpan
    (newState, p) <- next (fromSeal $ state headESpan) nextWord split' 
    return (headESpan {state = Open newState}, p) 
 

-- implementations of declarative rules
canOptLinkL span =  
      ((False, False)== hasParentPair span) && 
      (case state $ leftEnd span of Open _ -> True 
                                    _ -> False)

canOptLinkR span =  
      ((False, False)== hasParentPair span) && 
      (case state $ rightEnd span of Open _ -> True 
                                     _ -> False)



-- The OptLink Rules take spans with dual head (0,0) and adjoin the head on 
-- one side to the head on the other. 
--optLinkL'
optLinkL' span = do -- $ do
      guard $ canOptLinkL span
      (leftEnd', p) <- advance (leftEnd span) (word $ rightEnd span) 
      let pfin' = finish (fromSeal $ state $ rightEnd span) (fromJustNote "split" $ split $ rightEnd span) 
      Just pfin <- [pfin']
      return $ (span { simple = True, 
                       leftEnd = leftEnd',
                       rightEnd = (rightEnd span) {hasParent = True,
                                                  split = Nothing,
                                                  state = Sealed}
                     },
               p `times` pfin)

optLinkL :: (WFSM fsa) => SingleDerivationRule (EItem fsa)
optLinkL (span, semi) =do
  (span', semi') <- optLinkL' span
  return (span', semi `times` semi') 
    

--optLinkR' :: (WFSM fsa) => SingleDerivationRule (EItem fsa)
optLinkR' span = do 
  guard $ canOptLinkR span
  let pfin' = finish (fromSeal $ state $  leftEnd span) (fromJustNote "split" $ split $ leftEnd span) 
  Just pfin <- [pfin'] 
  (rightEnd', p) <- advance (rightEnd span)  (word $ leftEnd span)   
  return $ (span {simple = True, 
                  rightEnd = rightEnd',
                  leftEnd = (leftEnd span) {hasParent = True,
                                            split = Nothing,
                                            state = Sealed}
                 },
            p `times` pfin)

optLinkR :: (WFSM fsa) => SingleDerivationRule (EItem fsa)
optLinkR (span, semi) =do
  (span', semi') <- optLinkR' span
  return (span', semi `times` semi') 

{-# INLINE canCombine #-}
canCombine span1 span2 =
    simple span1 && (b2 /= b2') &&  f1 && f2 && w1 == w2
        where
          b2 = hasParent $ rightEnd span1
          b2'= hasParent $ leftEnd span2
          f1 = isStateFinal $ state $ rightEnd span1
          f2 = isStateFinal $ state $ leftEnd span2
          w1 = word $ rightEnd span1
          w2 = word $ leftEnd span2


-- Combine rules take a right finished simple span 
-- and merge it with a a left finished span. Producing a new span 
-- that is ready for an optlink adjunction  

combine' span1 span2 = do 
  guard  $ canCombine span1 span2
  Just pfin <- [pfin']
  return $ (ESpan {simple = False,
                  leftEnd = (leftEnd span1)   {split = leftSp},
                  rightEnd = (rightEnd span2) {split = rightSp}},
             pfin)
        where
          pfin' = if not b2' then -- _ 1 0 1
                     finish (fromSeal $ state $ leftEnd  span2) (fromJustNote "fina" $ split $ leftEnd  span2)
                  else if not b2 then 
                     finish (fromSeal $ state $ rightEnd span1) (fromJustNote "finb" $ split $ rightEnd  span1)   
                  else one
          (leftSp,rightSp) = if (not b1) && b2 then
                                 (split $ leftEnd span2,  split $ rightEnd span2)
                             else if not b3 && b2' then
                                 (split $ leftEnd span1,  split $ rightEnd span1)
                             else (Nothing, Nothing)
          (b1, b2)  = hasParentPair span1
          (b2', b3) = hasParentPair span2

combine :: (WFSM fsa) => DoubleDerivationRule (EItem fsa)
combine (span1, semi1) (span2, semi2) = do  
  (span', semi') <- combine' span1 span2
  return (span', semi' `times` semi1 `times` semi2) 

singleEnd :: (WFSM fsa) => 
             Int -> 
             fsa -> 
             Sym fsa ->
            [(ESpanEnd fsa, Semi fsa)]
singleEnd i fsa word = do 
    (state, semi)  <- initialState $ fsa
    return $ (ESpanEnd {                  
                 state = Open state,
                 word = word,
                 hasParent = False,
                 split = Just i
              }, 
             semi)

-- Seed 
seed :: (WFSM fsa) => 
        GetFSM fsa -> 
        Int ->
       [Sym fsa] -> 
       [Sym fsa] -> 
       InitialDerivationRule (EItem fsa)
seed getFSA i sym1s sym2s = do  
      sym1 <- sym1s 
      sym2 <- sym2s
      let (_, rightFSA) = getFSA i sym1
      let (leftFSA, _) = getFSA (i+1) sym2
      (span1, semi1) <- singleEnd i rightFSA $ sym1
      (span2, semi2) <- singleEnd (i+1) leftFSA $  sym2
      return (ESpan {
                leftEnd = span1,
                rightEnd = span2,
                simple = True
              }, 
              semi1 `times` semi2) 
    

seal :: (WFSM fsa) => EItem fsa -> Maybe (EItem fsa) 
seal (span, semi) =
    if not $ any (isStateFinal.state) [leftEnd span, rightEnd span] then
        Nothing
    else 
        Just (newESpan, semi)
    where
      newESpan = span{ leftEnd = trySeal $ leftEnd span,
                       rightEnd = trySeal $ rightEnd span} 
      trySeal spanEnd = 
              if isStateFinal $ state $ spanEnd then
                  spanEnd -- { state = Sealed }
              else spanEnd
                 

accept :: (WFSM fsa) => EItem fsa -> Bool
accept (span, _) = 
    b == (True, False) && f1 && f2 
        where
          b =  hasParentPair span
          f1 = isStateFinal (state $ rightEnd span)
          f2 = isSealed (state $ leftEnd span)

type GetFSM fsa = Int -> Sym fsa -> (fsa, fsa) --todo: fix this 

processCell :: ( Show (FSMSemiring (State fsa)), WFSM fsa, WordLattice sent) => 
               GetFSM fsa -> 
               sent ->  
               (Symbol sent -> Sym fsa) ->     
               ([EItem fsa] -> [EItem fsa]) ->            
               Span -> -- Size of the cell 
               (Span -> [EItem fsa]) -> -- function from cell to contenst 
               ([EItem fsa] -> [EItem fsa]) -> 
               [EItem fsa] -- contents of the new cell 
processCell getFSA sentence wordConv beamFirst (i, k) chart beam = 
   if k > latticeLength sentence then []
   else
    catMaybes $ map seal $ 
    if k-i == 1 then        
        (let seedCells = beam $ beamFirst {-$ M.toList $ M.fromListWith mappend -} $ seed getFSA i 
                        (map wordConv $ getWords sentence i) 
                        (map wordConv  $ getWords sentence (i+1))
        in
          seedCells ++ left seedCells ++ right seedCells)
    else
        (left starter ++ right starter ++ starter) 
        where
          starter =  beam $ M.toList $ M.fromListWith mappend $ concat [combine s1 s2
                            | j  <- [i+1..k-1],
                              s2 <- chart (j,k),
                              s1 <- chart (i,j)]
          left = beam . concatMap (\s ->  optLinkL s)  
          right = beam . concatMap (\s -> optLinkR s )  

eisnerParse :: ( Show (FSMSemiring (State fsa)), WFSM fsa, WordLattice sent) => 
               GetFSM  fsa -> 
               (Symbol sent -> Sym fsa) -> 
               sent  ->                 
              (Span -> M.Map (ESpan fsa) (Semi fsa) -> M.Map (ESpan fsa) (Semi fsa)) ->
              ([EItem fsa] -> [EItem fsa]) -> 
              ([EItem fsa] -> [EItem fsa]) -> 
               (Maybe (Semi fsa), Chart (ESpan fsa) (Semi fsa))
eisnerParse getFSM wordConv sent prune beam beamFirst = (semi, chart)  
    where chart = chartParse sent (processCell getFSM sent wordConv beamFirst) prune beam
          semi = do
            last <- chartLookup (1, latticeLength sent) chart
            let semiresults = map snd $ filter accept last
            
            guard (length semiresults > 0)  
            return $ mconcat semiresults 
