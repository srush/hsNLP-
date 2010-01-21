{-# LANGUAGE ScopedTypeVariables #-}
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
import ExtraParams
import NLP.ChartParse.Outside
import NLP.Probability.ConditionalDistribution
import Common
separate :: (Eq el) => el -> [el] -> [[el]]
separate el [] = [] 
separate el ls = case elemIndex el ls of
                   Just n -> 
                       (take n ls): (separate el (drop (n+1) ls))
                   Nothing -> []


main = do 
  [adjCountFile, spineCountFile, spineProbFile, testFile, infile, outfile] <- getArgs

  counts <- decodeFile adjCountFile
  spineCounts <- decodeFile spineCountFile
  spineCounts2 <- decodeFile spineProbFile
  let probSpine = estimatePrior spineCounts2
  let probs = estimateTAGProb (counts::TAGCounts)
  
  contents <- readFile testFile
  hSetBuffering stdout NoBuffering
  let sents =  map (parseSentence testFile. unlines) $ 
               separate "" $ lines contents

  extraprobs <- if infile == "r" then
                     return $ randomInitial
                 else
                     estimateFlipProbs `liftM` decodeFile infile
  
  let results = [ (b, chart , outchart, aligned)
          | 
     
            (Just b, chart, outchart, aligned) <- map (parseSent counts (spineCounts::SpineExist) probs extraprobs probSpine) sents]
  
  --putStrLn $ chartStats $ snd $ head results

  let counts = mconcat $ map (\(a::Prob, chart, outchart, aligned) ->  
           --putStrLn $ (show chart) ++"Outside \n"  ++ (show outchart) ++ (render $ pPrint $ collectCounts a aligned)) --("G " ++ (show a) ++ (show $ chart) ++ "\nO\n" ++ (show $ outchart) ++ (show $ collectCounts a aligned)))
         collectCounts a aligned  ) results 
  encodeFile outfile counts 
  putStrLn $ show counts



collectCounts (Prob z) aligned = mconcat $ do  
    (sig, (Prob insemi, Prob outsemi)) <- aligned
    let leftFakeSide = case state $ leftEnd sig of 
                         EI.Sealed -> Nothing
                         Open a -> Just $ SideEvent $ stateSide a
        rightFakeSide = case state $ rightEnd sig of 
                         EI.Sealed -> Nothing
                         Open a -> Just $ SideEvent $ stateSide a
        leftContext = SideContext (twSpine $ fromJustNote "context" $ EI.word $ leftEnd sig) (twWord $ fromJustNote "context" $  EI.word $ leftEnd sig)  (ARight)
        rightContext = SideContext (twSpine $ fromJustNote "context" $ EI.word $ rightEnd sig) (twWord $ fromJustNote "context" $ EI.word $ rightEnd sig)  (ALeft)
 
    obs  <- catMaybes [do {lf <-leftFakeSide; 
                           (if ((insemi * outsemi) / z) > 1 then
                                trace (show sig) $ trace (show insemi) $ trace (show outsemi) $ trace (show $ insemi * outsemi) $  trace (show z)
                           else id )
                               return $ condObservations lf leftContext ((insemi * outsemi) / z)},
                       do {rf <- rightFakeSide;
                           (if ((insemi * outsemi) / z) > 1 then
                                trace (show sig) $ trace (show insemi) $ trace (show outsemi) $ trace (show $ insemi * outsemi) $  trace (show z)
                           else id )
                           return $ condObservations rf rightContext ((insemi * outsemi) / z)}]
    return obs


parseSent counts spineCounts probs extraprobs probSpine insent = 
    (case b of 
      Nothing -> trace (show b)
      Just  _ -> id )
          (b, chart, outchart, aligned)
    where dsent = toTAGDependency insent
          aligned = alignCharts sent chart outchart 
          outchart = eisnerOutside sent chart 
          (b, chart)= eisnerParse (getFSM trivVal True) symbolConv sent
                           (const id)  (globalThresOne probSpine 0.0)
          (TAGSentence actualsent dep) = dsent
          sent = toTAGTest spineCounts insent   
          ldiscache = mkDistCacheLeft actualsent
          rdiscache = mkDistCacheRight actualsent
          trivVal :: Validity
          trivVal _ _ _ _ = True
          prunVal :: Validity
          prunVal = valid dsent

          getFSM val collins i (Just word) =  (initAdj (ProbModel probs extraprobs val) ldiscache ALeft word collins,
                                               initAdj (ProbModel probs extraprobs val) rdiscache ARight word collins)

          symbolConv word = Just word 
                         


globalThres n wher m =
    M.filter (\(Prob p) -> p >= n/100000) $  m    

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
          where (Prob p) = i 


