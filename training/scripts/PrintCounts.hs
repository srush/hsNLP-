import NLP.Model.TAG.Adjunction
import System (getArgs) 
import System.IO
import Data.Binary
import Data.Monoid
import Text.PrettyPrint.HughesPJClass
import NLP.Probability.Chain
import NLP.ParseMonad
main = do 
  [countFile] <- getArgs
  counts <- decodeFile countFile
  dm <- loadDebugMappers
  putStrLn $ render $ runParseMonad (dumpCollinsObs (counts::Observation Collins)) dm