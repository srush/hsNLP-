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
import NLP.ChartParse.Eisner
import NLP.Semiring.ViterbiNBestDerivation
import Safe (fromJustDef, fromJustNote, at)
import DependencyStructure
import Debug.Trace
import Text.PrettyPrint.HughesPJClass
import Text.Printf
import Data.Array
import Control.Parallel.Strategies

separate :: (Eq el) => el -> [el] -> [[el]]
separate el [] = [] 
separate el ls = case elemIndex el ls of
                   Just n -> 
                       (take n ls): (separate el (drop (n+1) ls))
                   Nothing -> []


main = do 
  [adjCountFile, spineCountFile, spineProbFile, testFile] <- getArgs
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
  let results = [  (b''', b)
          | (_, (Just b''', _) , (Just b', _),(Just b, _)) <- map (parseSent counts (spineCounts::SpineExist) probs probSpine) sents]

  mapM_ (\(a,b) ->  do
         putStrLn $ ("G" ++ (show $ tagDerToTree  $ getBestDerivation a))
--         putStrLn $ show $ getBestDerivation b
         putStrLn $ ("T" ++ (show $ tagDerToTree  $ getBestDerivation b))) results
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
          where 
       

parseSent counts spineCounts probs probSpine insent = (dep, 
                                                       eisnerParse getFSM symbolConv actualsent (specialPrune insent),
                                                       eisnerParse getFSM symbolConv actualsent (prune probSpine),
                                                       eisnerParse getFSM symbolConv sent (prune probSpine))
    where dsent = toTAGDependency insent
          (TAGSentence actualsent dep) = dsent
          sent = toTAGTest spineCounts insent   
          getFSM i (Just word) =  (initAdj (head . getWords sent) probs ALeft word,
                                   initAdj (head . getWords sent) probs ARight word)
          symbolConv word = Just word 
                          
specialPrune :: WordInfoSent -> Range -> M.Map (Span AdjState)  semi -> M.Map (Span AdjState) semi
specialPrune (WordInfoSent wisent) (i,k') m = --trace (show (i, k', n) ) $
     M.filterWithKey killNonSeen m 
         where  killNonSeen sig _ =
                    case  hasParentPair sig  of 
                      (True, False) ->  not (simple sig) || (adjoinInd (wisent ! lefty) == 
                                        endy righty)
                      (False, True) -> not (simple sig) || endy righty == 0 || (adjoinInd (wisent ! righty) == 
                                        endy lefty)
                      _ -> True
                    where righty = twInd $ fromJustNote "" $ NLP.ChartParse.Eisner.word $ rightEnd $ sig
                          lefty = twInd $ fromJustNote "" $ NLP.ChartParse.Eisner.word $ leftEnd $ sig
                (_,n) = bounds wisent
                endy k' = if k' > n then 0 else k'  

  --print $ show (counts::TAGTrainingCounts) 
--prune :: (Ord sig) => M.Map sig (ViterbiDerivation TAGDerivation) -> 
--                     M.Map sig (ViterbiDerivation TAGDerivation)  
prune probs wher m = --trace ((printf "Best for %s is : %s " (show wher ) (show best)) ++ ("\nBadies\n" ++ (show $ Cell p)) ++ ("\ngoodies\n" ++(show $ Cell p'))) $ s 
 s
     where 
      s = M.filterWithKey (\sig semi -> (getFOM (sig,semi)) > (best / 1000)) m    
      p' = M.filter (\(_,fom) -> fom >= (best / 1000)) $ M.mapWithKey (\sig semi -> (semi, getFOM (sig,semi))) m    
      p = M.filter (\(_,fom) -> fom <= (best / 1000)) $ M.mapWithKey (\sig semi -> (semi, getFOM (sig,semi))) m    
      getProb = probSpine probs 
      getPrior sig = (prior getProb $ state $ leftEnd sig) * (prior getProb $ state $ rightEnd sig) 
      getInside i  =  p
          where (Prob p) = getBestScore i 
      getFOM (sig, semi) = getPrior sig * getInside semi
      hasAdjoin (sig, semi) = hasParentPair sig /= (False, False) 
      best = case  map getFOM $ filter hasAdjoin $ M.toList m of
               [] -> 0.0
               ls -> maximum $ ls
--
    --where best = 
