module NLP.ChartParse.Outside where 
import NLP.FSM
import NLP.Semiring
import NLP.ChartParse.Eisner
import NLP.ChartParse
import Data.Monoid.Multiplicative (times, one) 
import Control.Monad
import Debug.Trace
wordPair span = 
    (word $ leftEnd span , word $ rightEnd span) 

--unOptL :: (WFSM fsa) =>  SingleDerivationRule (EItem fsa) 
unOptL (inspan, isemi) (outspan, osemi) = do
  guard $ hasParentPair outspan == (False, True)
  guard $ simple outspan
  (inspanLinked, linkedp) <- optLinkL' inspan
  guard $ outspan == inspanLinked
  return $ (inspan,
            linkedp `times` osemi)

unOptR (inspan, isemi) (outspan, osemi) = do
  guard $ hasParentPair outspan == (True,False) 
  guard $ simple outspan
  (inspanLinked, linkedp) <- optLinkR' inspan
  guard $ outspan == inspanLinked
  return $ (inspan,
            linkedp `times` osemi)

wordMatch inspan1 inspan2 outspan = 
    iw1 == ow1 && iw4 == ow2  
    where
      (iw1,_) = wordPair inspan1
      (_,iw4) = wordPair inspan2
      (ow1,ow2) = wordPair outspan
    
unCombineR (inspan1,semi1) (inspan2, semi2) (outspan, osemi) = do 
    guard $ (not $ simple outspan) && wm  
    (incombineSpan, pcomb) <- combine' inspan1 inspan2
    guard $ incombineSpan == outspan  
    --let newState = flip $ state $ leftEnd inspan2
    return $ 
         (Span {simple = simple inspan2,
                leftEnd = leftEnd inspan2,    
                rightEnd = rightEnd outspan
               } 
         , osemi `times` semi1 `times` pcomb
         )
        
    where wm = wordMatch inspan1 inspan2 outspan

unCombineL deb (inspan1,_) (inspan2, semi2) (outspan, osemi) = do 
    guard $ not $ simple outspan &&  wm
    (incombineSpan, pcomb) <- (if deb then trace (show inspan1) $ trace (show inspan2) $ trace (show outspan) else id)  combine' inspan1 inspan2
    guard $ incombineSpan == outspan  
    return $ 
        (Span {simple = True,  
                leftEnd = leftEnd outspan,
                rightEnd = rightEnd inspan1
               },
          osemi `times` semi2 `times` pcomb)
    where wm = wordMatch inspan1 inspan2 outspan



unprocessCell :: (WFSM fsa, SentenceLattice sent) => 
               sent -> 
               Range -> -- Size of the cell 
               (Range -> [EItem fsa]) -> -- function from cell to contenst
               (Range -> [EItem fsa]) -> 
               [EItem fsa] -- contents of the new cell 
unprocessCell sentence (i, j) inChart outChart = seeds
     ++  concat [unOptR s1 o | s1 <- inChart(i,j), o <- seeds] 
     ++  concat [unOptL s1 o | s1 <- inChart(i,j), o <-seeds]            
    where 
      --seeds :: [(EItem ,Semi fsa) ]
      seeds = if (i,j) == (1, n + 1) then 
                 do
                   (inspan, _) <- inChart (i,j)
                   guard $ hasParentPair inspan == (True, False) 
                   return $ (inspan, one) 
             else if (i,j) == (1,1) then
                      trace "1 3" $ 
                      let b = concat $ concat $ (
                                         [uncomb (unCombineL True) i j k | k <- [j+1..n+1]] ++ 
                                         [uncomb unCombineR h i j | h <- [1..i-1]])
                      in b
                  else
                      concat $ concat $ (
                                         [uncomb (unCombineL False) i j k | k <- [j+1..n+1]] ++ 
                                         [uncomb unCombineR h i j | h <- [1..i-1]])
      uncomb fn i j k = 
          [ fn s1 s2 o | 
            s1 <- inChart (i, j),
            s2 <- inChart (j, k),
            o <- outChart (i, k)
          ]
      n = sentenceLength sentence

eisnerOutside sent inside = 
    outsideParse sent inside (unprocessCell sent)