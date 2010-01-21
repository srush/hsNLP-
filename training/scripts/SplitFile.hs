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
  [file1, n, file2] <- getArgs
  contents <- readFile file1
  let sents =  separate "" $ lines contents
  let nsents = ngroup sents $ read n
  print "full count done"
  mapM_ (output file2) $ zip nsents [0..]
  
output file2 (sents, i) =
    writeFile (file2++ (show i) ++ ".data") ((intercalate "\n" $ map unlines sents) ++ "\n") 
