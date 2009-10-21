import TreeBank 
import Control.Exception
import Prior
import TAG 
import TAGparse
import Adjunction
import System (getArgs) 
import System.IO
import Data.Monoid
import Data.Binary
import Data.List
import qualified Data.Map as M
import Sentence 
import NLP.ChartParse
import NLP.Semiring
import NLP.Semiring.Prob
import NLP.ChartParse.Eisner as EI
import NLP.Semiring.ViterbiNBestDerivation
import Safe (fromJustDef, fromJustNote, at)
import DependencyStructure
import Debug.Trace
import Text.PrettyPrint.HughesPJClass
import Text.Printf
import Data.Array
import Control.Parallel.Strategies
import Distance
import Data.Algorithm.Diff
separate :: (Eq el) => el -> [el] -> [[el]]
separate el [] = [] 
separate el ls = case elemIndex el ls of
                   Just n -> 
                       (take n ls): (separate el (drop (n+1) ls))
                   Nothing -> []


main = do 
  [adjCountFile, spineCountFile, spineProbFile, testFile, n] <- getArgs
  counts <- decodeFile adjCountFile
  spineCounts <- decodeFile spineCountFile
  spineCounts2 <- decodeFile spineProbFile
  let probSpine = estimatePrior spineCounts2
  --print counts
  let probs = estimateTAGProb (counts::TAGCounts)
  
  contents <- readFile testFile
  hSetBuffering stdout NoBuffering
  let sents =  map (parseSentence testFile. unlines) $ 
               separate "" $ lines contents
                        --print sents
  --print sents
  
  let results = [  ((b', b),chart)
          | (n, (_, (Just b', _) , _, (Just b, chart))) <- zip [1..] $ map (parseSent counts (spineCounts::SpineExist) probs probSpine) sents]
  
  putStrLn $ chartStats $ snd $ head results

  mapM_ (\((a,b),_) ->  do
           let der1 = getBestDerivation a
           let der2 = getBestDerivation b
           let (Prob sc1) = getBestScore a
           let (Prob sc2) = getBestScore b
           let TAGDerivation (_, debug1) = der1  
           let TAGDerivation (_, debug2) = der2
           let m1 = M.fromList debug1
           let m2 = M.fromList debug2
           let diff1 = M.difference m1 m2 
           let diff2 = M.difference m2 m1
           let st1 = (render $ niceParseTree $ tagDerToTree der1)
           let st2 = (render $ niceParseTree $ tagDerToTree der2)
           let ldiff  = getDiff (lines st1) (lines st2) 
           putStrLn $ "First " ++ (printf "%.3e" sc1) 
           putStrLn $ render $ vcat $ map (pPrint . snd) $ M.toList diff1
           putStrLn $ "Second" ++ (printf "%.3e" sc2) 
           putStrLn $ render $ vcat $ map (pPrint . snd) $ M.toList diff2
           putStrLn $ show ldiff
           putStrLn $ st1
           putStrLn $ st2
           --putStrLn $ show $ getBestDerivation b

           putStrLn $ ("G" ++ " " ++ (show $ tagDerToTree der1))
           putStrLn $ ("T" ++ " " ++ (show $ tagDerToTree der2))) results

      --print "The fixed parse answer"
  --putStrLn $ ("G" ++ (show $ tagDerToTree  $ getBestDerivation b'''))
  --putStrLn $ ("T" ++ (show $ tagDerToTree  $ getBestDerivation b))
 -- print "The fixed spine answer"
 -- print $ tagDerToTree $ getBestDerivation  b' 
 -- print "The test answer"
  --print $ tagDerToTree $ getBestDerivation  b 

  --let (TAGDerivation dep)= getBestDerivation b
  --print $ TAGDerivation (dep)
  --let wrong = score d (fmap (\adj -> AdjunctionInfo{TAG.adjPos = TAG.adjPos adj, adjType = adjType adj,adjInfo = ()})  dep)
  --if length wrong == 0 then putStrLn "Perfect."
   --else putStrLn $ render $ vcat $ map (text.show) wrong
          
       

parseSent counts spineCounts probs probSpine insent = 
    (dep, 
     (Just b, chart),
     (),
     (Just b', chart'))
    where dsent = toTAGDependency insent
          sc1 =  getBestScore b
          (Just b, chart) = eisnerParse (getFSM prunVal False) symbolConv actualsent (\ wher m -> globalThres 0.0 wher m) id
          (Just b',chart')= eisnerParse (getFSM trivVal True) symbolConv sent (\ wher m -> prune probSpine wher $ globalThres sc1 wher m)
                            (globalThresOne probSpine sc1)
          (TAGSentence actualsent dep) = dsent
          sent = toTAGTest spineCounts insent   
          ldiscache = mkDistCacheLeft actualsent
          rdiscache = mkDistCacheRight actualsent
          trivVal :: Validity
          trivVal _ _ _ _ = True
          prunVal :: Validity
          prunVal = valid dsent
          getFSM val collins i (Just word) =  (initAdj (probs, val) ldiscache ALeft word collins,
                                               initAdj (probs, val) rdiscache ARight word collins)

          symbolConv word = Just word 
                          
-- specialPrune :: WordInfoSent -> Range -> M.Map (Span (AdjState TAGProbs))  semi -> M.Map (Span (AdjState TAGProbs)) semi
-- specialPrune (WordInfoSent wisent) (i,k') m = --trace (show (i, k', n) ) $
--      M.filterWithKey killNonSeen m 
--          where  killNonSeen sig _ =
--                     case  hasParentPair sig  of 
--                       (True, False) ->  not (simple sig) || ((adjoinInd (wisent ! lefty) == 
--                                         endy righty) && )
--                       (False, True) -> not (simple sig) || endy righty == 0 || (adjoinInd (wisent ! righty) == 
--                                         endy lefty)
--                       _ -> True
--                     where righty = twInd $ fromJustNote "" $ EI.word $ rightEnd $ sig
--                           lefty = twInd $ fromJustNote "" $ EI.word $ leftEnd $ sig
--                 (_,n) = bounds wisent
--                 endy k' = if k' > n then 0 else k'  


globalThres n wher m =
    M.filter (\p -> getBestScore p >= n/100000) $  m    

globalThresOne probs (Prob n) ps =  
    filter (\p -> score p >= (n/100000) && score p >= (best/10000))  ps  
        where
          score p = getFOM probs p 
          best = case  map (getFOM probs) ps of
               [] -> 0.0
               ls -> maximum $ ls



getFOM probs (sig, semi) = getPrior sig * getInside semi 
    where
      getProb = probSpine probs 
      getPrior sig = (if hasParent $ leftEnd sig then 1.0 else (prior getProb $ EI.word $ leftEnd sig)) * 
               (if hasParent $ rightEnd sig then 1.0 else (prior getProb $ EI.word $ rightEnd sig)) 
      getInside i  =  p
          where (Prob p) = getBestScore i 

  --print $ show (counts::TAGTrainingCounts) 
--prune :: (Ord sig) => M.Map sig (ViterbiDerivation TAGDerivation) -> 
--                     M.Map sig (ViterbiDerivation TAGDerivation)  
prune probs wher m = 
    --(trace ((printf "Best for %s is : %s %s %s  " (show wher ) (show bestNH) (show bestR) (show bestL)) ++ (show (Cell s))) )  
 s 
     where 
      s = M.filterWithKey (\sig semi -> ( -- getFOM (sig,semi)) > (best / 10000) --
                                         if hasNoAdjoin (sig,semi) then
                                             getFOM probs (sig,semi) >= (bestNH / 10000) || isNaN bestNH 
                                         else if hasAdjoinL (sig,semi) then
                                             getFOM probs (sig,semi) >= (bestL / 10000) || isNaN bestL
                                         else if hasAdjoinR (sig,semi) then
                                             getFOM probs (sig,semi) >= (bestR / 10000) || isNaN bestR 
                                        else True)
                                        --    (getFOM (sig,semi)) > (bestH / 50000)
                                        --else  (getFOM (sig,semi)) > (bestNH / 50000)
                          ) m    
      --p' = M.filter (\(_,fom) -> fom >= (bestH / 50000)) $ M.mapWithKey (\sig semi -> (semi, getFOM (sig,semi))) m    
      --p = M.filter (\(_,fom) -> fom <= (bestH / 50000)) $ M.mapWithKey (\sig semi -> (semi, getFOM (sig,semi))) m    

      hasNoAdjoin (sig, semi) = hasParentPair sig == (False, False) 
      hasAdjoinL (sig, semi) = hasParentPair sig == (True, False) 
      hasAdjoinR (sig, semi) = hasParentPair sig == (False, True) 
      best = case  map (getFOM probs )$ M.toList m of
               [] -> 0.0
               ls -> maximum $ ls
      bestL = case  map (getFOM probs) $ filter hasAdjoinL $ M.toList m of
               [] -> 0.0
               ls -> maximum $ ls
      bestR = case  map (getFOM probs) $ filter hasAdjoinR $ M.toList m of
               [] -> 0.0
               ls -> maximum $ ls

      bestNH = case  map (getFOM probs) $ filter hasNoAdjoin $ M.toList m of
               [] -> 0.0
               ls -> maximum $ ls

--
    --where best = 
