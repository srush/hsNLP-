import TreeBank 
import TAG 
import System (getArgs) 
import System.IO
import Data.Monoid
import Data.Binary
import Data.List

import NLP.ChartParse.Eisner

separate :: (Eq el) => el -> [el] -> [[el]]
separate el [] = [] 
separate el ls = case elemIndex el ls of
                   Just n -> 
                       (take n ls): (separate el (drop (n+1) ls))
                   Nothing -> []


main = do 
  [countFile, testFile] <- getArgs
  counts <- decodeFile countFile
  let probs = estimateTAGProb counts
  contents <- readFile testFile
  let sents =  map (parseSentence testFile. unlines) $ 
               separate "" $ lines contents
  putStrLn $ show $ map (parseSent probs) sents 
          where parseSent probs insent = eisnerParse getFSM symbolConv sent 
                    where dsent = toTAGDependency (initSemiProbs probs) insent
                          (TAGSentence sent _) = dsent
                          getFSM i (Just word) = (initAdj (leftProbs probs) word,
                                                  initAdj (rightProbs probs) word)
                          symbolConv word = Just word 
                          
  --print $ show (counts::TAGTrainingCounts) 
