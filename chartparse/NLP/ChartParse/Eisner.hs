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

type EisnerChart fsa = Chart (Span fsa) (FSMSemiring (State fsa))
type Semi fsa = FSMSemiring (State fsa)
type Sym fsa = FSMSymbol (State fsa)
type EItem fsa = Item (Span fsa) (Semi fsa) 


-- Data structure from p. 12 declarative structure  
data SpanEnd fsa =
    SpanEnd {
      hasParent :: Bool, -- b1 and b2 (does the parent exist in the span, i.e. it's not the head)     
      state :: State fsa, -- q1 and q2
      word :: Sym fsa,
      headTil :: (Int, Int) -- the "last" point in the span where this word is the head
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
      rightEnd :: SpanEnd fsa
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
advance :: (WFSM fsa) => SpanEnd fsa -> Sym fsa -> 
           [(SpanEnd fsa, Semi fsa)] 
advance headSpan nextWord = do 
    (newState, p) <- next (state headSpan) nextWord 
    return (headSpan {state = newState}, p) 
 


-- implementations of declarative rules

-- The OptLink Rules take spans with dual head (0,0) and adjoin the head on 
-- one side to the head on the other. 
optLinkL :: (WFSM fsa) => SingleDerivationRule (EItem fsa)
optLinkL (span, semi) = do --trace "optLinkL" $ do
      (False, False) <- [hasParentPair span]
      (leftEnd', p) <- advance (leftEnd span) (word $ rightEnd span)   
      return $ (span { simple = True, 
                       leftEnd = leftEnd' {headTil = (fst $ headTil $ leftEnd $ span, 
                                                      snd $ headTil $ rightEnd $ span)} ,
                       rightEnd = (rightEnd span) {hasParent = True}
                     },
               p `times` semi)


optLinkR :: (WFSM fsa) => SingleDerivationRule (EItem fsa)
optLinkR (span, semi) =  do --trace "optLinkR" $ do 
    (False, False) <- [hasParentPair span]
    (rightEnd', p) <- advance (rightEnd span) (word $ leftEnd span)   
    return  $ (span {simple = True, 
                      rightEnd = rightEnd' {headTil = (fst $ headTil $ leftEnd $ span, 
                                                       snd $ headTil $ rightEnd $ span)},
                      leftEnd = (leftEnd span) {hasParent = True}
                    },
                p `times` semi)

-- Combine rules take a right finished simple span 
-- and merge it with a a left finished span. Producing a new span 
-- that is ready for an optlink adjunction  
combine :: (WFSM fsa) => DoubleDerivationRule (EItem fsa)
combine (span1, semi1) (span2, semi2) = 
    if simple span1 && (b2 /= b2') && f1 && f2 && w1 == w2 then 
             [(Span {simple = False,
                    leftEnd = leftEnd span1,
                    rightEnd = rightEnd span2},
             semi1 `times` semi2)]
    else []
        where
          ((_, b2), (b2', _)) =  (hasParentPair span1, hasParentPair span2)
          f1 = isFinal (state $ rightEnd span1)
          f2 = isFinal (state $ leftEnd span2)
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
                 state = state,
                 word = word,
                 hasParent = False,
                 headTil = (i,i) 
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
                simple = True}, 
              semi1 `times` semi2) 
    
accept :: (WFSM fsa) => EItem fsa -> Bool
accept (span, _) = 
    b == (True, False) && f1 && f2 
        where
          b =  hasParentPair span
          f1 = isFinal (state $ rightEnd span)
          f2 = isFinal (state $ leftEnd span)


type GetFSM fsa = Int -> Sym fsa -> (fsa, fsa) --todo: fix this 

processCell :: (WFSM fsa, SentenceLattice sent) => 
               GetFSM fsa -> 
               sent ->  
               (Symbol sent -> Sym fsa) ->                
               Range -> -- Size of the cell 
               (Range -> [EItem fsa]) -> -- function from cell to contenst 
               [EItem fsa] -- contents of the new cell 
processCell getFSA sentence wordConv (i, k) chart = concat $ 
    --trace(show (i,k)) $! 
    if k-i == 1 then
        let seedCells = seed getFSA i 
                        (map wordConv $ getWords sentence i) 
                        (map wordConv  $ getWords sentence (i+1))
        in
        map (\seedCell -> 
        concat $ [[seedCell],
         optLinkL seedCell,
         optLinkR seedCell]) seedCells
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
            (_, semi) <- find accept last
            return semi 
