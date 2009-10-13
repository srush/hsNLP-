{-# LANGUAGE TypeFamilies, ExistentialQuantification, FlexibleContexts #-}
module NLP.ChartParse.Eisner where 
import NLP.ChartParse
import Data.Function (on)
import Data.List (intercalate, find)
import NLP.FSM
import NLP.Semiring
import qualified Data.Map as M
import Data.Monoid.Multiplicative (times, one) 
import Data.Maybe (catMaybes)
import Text.Printf
import Text.PrettyPrint.HughesPJClass
import Debug.Trace
import Safe (fromJustNote)
import Data.Maybe (maybe)
import Control.Exception
import Control.Monad

type EisnerChart fsa = Chart (Span fsa) (FSMSemiring (State fsa))
type Semi fsa = FSMSemiring (State fsa)
type Sym fsa = FSMSymbol (State fsa)
type EItem fsa = Item (Span fsa) (Semi fsa) 


data Seal a = Sealed | Open a
              deriving (Eq, Ord, Show)

isSealed (Sealed)= True
isSealed _ = False

isStateFinal (Open a) = isFinal a  
isStateFinal (Sealed) = True -- Check this! 


fromSeal (Open a) = a 

 
-- Data structure from p. 12 declarative structure  
data SpanEnd fsa =
    SpanEnd {
      hasParent :: Bool, -- b1 and b2 (does the parent exist in the span, i.e. it's not the head)     
      state :: Seal (State fsa), -- q1 and q2
      word  :: Sym fsa
}

instance (WFSM fsa) => Show (SpanEnd fsa) where 
    show end = intercalate " " 
               [(show $ hasParent end),
               (show $ state end),
               (show $ word end)] 

expandSpanEnd sp =  (hasParent sp, state sp, word sp)

instance (WFSM fsa) => Eq (SpanEnd fsa) where 
    (==)  = (==) `on` expandSpanEnd

instance (WFSM fsa) => Ord (SpanEnd fsa) where 
    compare = compare `on` expandSpanEnd 


data Span fsa =
    Span {
      simple :: Bool, -- s
      leftEnd :: SpanEnd fsa,
      rightEnd :: SpanEnd fsa,
      midSplit :: Maybe (Int, Int) -- (currently indexed -fix) the "last" point in the span where this word is the head
} deriving (Eq, Ord) 



instance (WFSM fsa) => Show (Span fsa) where
    show span = printf "s = %s b = %s s = %s wl = %s wr = %s" (showBool $ simple span) (showBoolPair $ hasParentPair span) (show (state $ leftEnd span, state $ rightEnd span)) (show $ word $ leftEnd span) (show $ word $ rightEnd span)
        where showBool True = "1"
              showBool False = "0"
              showBoolPair :: (Bool, Bool) -> String
              showBoolPair (a,b) = printf "(%s %s)" (showBool a) (showBool b) 
              

instance (WFSM fsa) => Pretty (Span fsa) where
    pPrint span = text $ printf "wl = %s wr = %s s = %s b = %s sim = %s" 
                (show $ word $ leftEnd span) 
                (show $ word $ rightEnd span)
                (show (state $ leftEnd span, state $ rightEnd span)) 
                (showBoolPair $ hasParentPair span)
                (showBool $ simple span)  
        where showBool True = "1"
              showBool False = "0"
              showBoolPair :: (Bool, Bool) -> String
              showBoolPair (a,b) = printf "(%s %s)" (showBool a) (showBool b) 

                               
hasParentPair span = 
    (hasParent $ leftEnd span , hasParent $ rightEnd span) 

-- Advances an internal WFSM (equivalent in this model to "adjoining" a new
-- dependency. 
advance :: (WFSM fsa) => SpanEnd fsa -> Int -> Sym fsa -> 
           [(SpanEnd fsa, Semi fsa)] 
advance headSpan split nextWord  = do 
    (newState, p) <- next (fromSeal $ state headSpan) nextWord split 
    return (headSpan {state = Open newState}, p) 
 


-- implementations of declarative rules

-- The OptLink Rules take spans with dual head (0,0) and adjoin the head on 
-- one side to the head on the other. 
optLinkL :: (WFSM fsa) => SingleDerivationRule (EItem fsa)
optLinkL (span, semi) = do --trace "optLinkL" $ do
      (False, False) <- [hasParentPair span]
      (Open _)  <- [state $ leftEnd span]
      (leftEnd', p) <- advance (leftEnd span) (fst $ fromJustNote "must be split" $ midSplit span) (word $ rightEnd span) 
      return $ (span { simple = True, 
                       leftEnd = leftEnd',
                       rightEnd = (rightEnd span) {hasParent = True},
                       midSplit = Nothing
                     },
               p `times` semi)


optLinkR :: (WFSM fsa) => SingleDerivationRule (EItem fsa)
optLinkR (span, semi) =  do --trace "optLinkR" $ do 
    (False, False) <- [hasParentPair span]
    (Open _)  <- [state $ rightEnd span]
    (rightEnd', p) <- advance (rightEnd span) (snd $ fromJustNote "must be split" $ midSplit span) (word $ leftEnd span)   
    return  $ (span {simple = True, 
                      rightEnd = rightEnd',
                      leftEnd = (leftEnd span) {hasParent = True},
                      midSplit = Nothing
                    },
                p `times` semi)

-- Combine rules take a right finished simple span 
-- and merge it with a a left finished span. Producing a new span 
-- that is ready for an optlink adjunction  
combine :: (WFSM fsa) => DoubleDerivationRule (EItem fsa)
combine (span1, semi1) (span2, semi2) = 
    if simple span1 && (b2 /= b2') && f1 && f2 && fOuter && w1 == w2 then 
             [(Span {simple = False,
                    leftEnd = leftEnd span1,
                    rightEnd = rightEnd span2,
                    midSplit = --trace ((show $ midSplit span2) ++ (show $ midSplit span1)) $ 
                               maybe (midSplit span2) Just (midSplit span1)  -- span1 is simple
                    },
             semi1 `times` semi2)]
    else []
        where
          ((_, b2), (b2', _)) =  (hasParentPair span1, hasParentPair span2)
          f1 = isSealed $ state $ rightEnd span1
          f2 = isSealed $ state $ leftEnd span2
          fOuter = any (isSealed.state) [leftEnd span1, rightEnd span2] -- for faster O(t) speedup 
          w1 = word $ rightEnd span1
          w2 = word $ leftEnd span2

singleEnd :: (WFSM fsa) => 
             Int -> 
             fsa -> 
             Sym fsa ->
            [(SpanEnd fsa, Semi fsa)]
singleEnd i fsa word = do 
    (state, semi)  <- initialState $ fsa
    return $ (SpanEnd {                  
                 state = Open state,
                 word = word,
                 hasParent = False
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
      return (Span {
                leftEnd = span1,
                rightEnd = span2,
                simple = True,
                midSplit = Just (i, i+1) 
              }, 
              semi1 `times` semi2) 
    

seal :: (WFSM fsa) => EItem fsa -> Maybe (EItem fsa) 
seal (span, semi) =
    if not $ any (isStateFinal.state) [leftEnd span, rightEnd span] then
        Nothing
    else 
        Just (newSpan, semi)
    where
      newSpan = span{ leftEnd = trySeal $ leftEnd span,
                       rightEnd = trySeal $ rightEnd span} 
      trySeal spanEnd = 
              if isStateFinal $ state $ spanEnd then
                  spanEnd { state = Sealed }
              else spanEnd
                 

accept :: (WFSM fsa) => EItem fsa -> Bool
accept (span, _) = 
    b == (True, False) && f1 && f2 
        where
          b =  hasParentPair span
          f1 = isStateFinal (state $ rightEnd span)
          f2 = isStateFinal (state $ leftEnd span)


type GetFSM fsa = Int -> Sym fsa -> (fsa, fsa) --todo: fix this 

processCell :: (WFSM fsa, SentenceLattice sent) => 
               GetFSM fsa -> 
               sent ->  
               (Symbol sent -> Sym fsa) ->                
               Range -> -- Size of the cell 
               (Range -> [EItem fsa]) -> -- function from cell to contenst 
               [EItem fsa] -- contents of the new cell 
processCell getFSA sentence wordConv (i, k) chart = catMaybes $ map seal $ concat $ 
    -- trace(show (i,k)) $! 
    if k-i == 1 then
        (let seedCells = seed getFSA i 
                        (map wordConv $ getWords sentence i) 
                        (map wordConv  $ getWords sentence (i+1))
        in
        map (\seedCell -> 
        concat $ [[seedCell],
                  optLinkL seedCell,
                  optLinkR seedCell]) seedCells)
    else
        concat $ 
        [let s = combine s1 s2 in
           [s, 
            s >>= optLinkL ,
            s >>= optLinkR ]
               | j  <- [i+1..k-1],
                s2 <- chart (j,k),
                s1 <- chart (i,j)]
         

eisnerParse :: (WFSM fsa, SentenceLattice sent) => 
               GetFSM  fsa -> 
               (Symbol sent -> Sym fsa) -> 
               sent  ->                 
              (Range -> M.Map (Span fsa) (Semi fsa) -> M.Map (Span fsa) (Semi fsa)) -> 
               (Maybe (Semi fsa), Chart (Span fsa) (Semi fsa))
eisnerParse getFSM wordConv sent prune = (semi, chart)  
    where chart = chartParse sent (processCell getFSM sent wordConv) prune
          semi = do
            last <- chartLookup (1, sentenceLength sent + 1) chart
            let semiresults = map snd $ filter accept last
            
            guard (length semiresults > 0)  
            return $ mconcat semiresults 
