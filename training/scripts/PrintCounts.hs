import Adjunction
import System (getArgs) 
import System.IO
import Data.Binary
import Data.Monoid
import Text.PrettyPrint.HughesPJClass

main = do 
  [countFile] <- getArgs
  counts <- decodeFile countFile
  putStrLn $ render $ pPrint (counts::TAGCounts)