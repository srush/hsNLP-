import TreeBank 
import TAG 
import Adjunction
import TAGparse
import System (getArgs) 
import System.IO
import Data.Monoid
import Data.Binary
import Data.List
import NLP.ChartParse.Eisner
import NLP.Semiring.Derivation
import Control.Exception
import Control.Parallel.Strategies
import DataHelpers

main = do 
  [file1, file2] <- getArgs
  counts <- readAndCount file1 file2
  print "full count done"
  encodeFile file2 (counts::TAGCounts)


readAndCount file1 file2 = do
  newsents <- getSentences file1
  print "ending parsing" 
  let counts = parMap rwhnf countSome $ zip newsents [0..]
  print "count sets done"
  return $! mconcat counts
      where 
        countSome (ls,n) = 
            --do 
          --putStrLn $ "set" ++ (show n)
          --return $! 
          mconcat $ map (directCounts2 . toTAGDependency) ls 
            

readCounts file = 
    decodeFile file
directCounts2 dsent =
    case semi of 
        Nothing -> throw $ AssertionFailed $ show dsent
        Just s -> fromDerivation s

    where 
      (TAGSentence sent _) = dsent
  --print sent
      fsms = tagSentenceFSMs dsent
      getFSM i _ = fsms !! (i-1)
      symbolConv word = Just word 
      (semi,chart) =   eisnerParse getFSM symbolConv sent id 
      