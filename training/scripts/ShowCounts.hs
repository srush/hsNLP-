import TreeBank 
import TAG 
import System (getArgs) 
import System.IO
import Data.Monoid
import Data.Binary
import Data.List


initSemi (TAGTrainingCounts (init ,_))=  


main = do 
  [countFile, testFile] <- getArgs
  counts <- decodeFile countFile
  let prob = esitimateTAGProb counts
  contents <- readFile testFile
  let sents =  map (parseSentence testFile. unlines) $ separate "" $ lines contents
  let tagDependency = map (toTAGDependency $ semi prob) sents 
  

  print $ show (counts::TAGTrainingCounts) 