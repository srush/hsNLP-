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
import Debug.Trace
import Counts 
main = do 
  [file1, file2] <- getArgs
  counts <- readAndCount file1 file2
  print "full count done"
  encodeFile file2 (counts::TAGCounts)

readAndCount file1 file2 = do
  newsents <- getSentences file1
  print "ending parsing" 
  let counts =  parMap rwhnf countSome $ zip newsents [0..]
  --print counts
  print "count sets done"
  return $! mconcat counts
      where 
        countSome (ls,n) =  
          --return $! 
          trace (show n) $ 
                mconcat $ map (countTAG . toTAGDependency) ls 
        
            
