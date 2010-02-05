import NLP.TreeBank.TreeBank 
import NLP.Model.TAG.Format
import NLP.Model.TAG.Parse
import System (getArgs) 
import System.IO
import Helpers.Common
import Data.Binary
import Data.Array
import qualified Data.Map as M
import qualified Data.Set as S
import NLP.ChartParse.Eisner.Inside hiding (word)
import NLP.Semiring.Derivation
import Control.Exception
import Control.Parallel.Strategies
import DataHelpers
import NLP.ParseMonad

main = do 
  [file1, file2] <- getArgs
  sentbundle <- getSentences file1
  dm <- loadDebugMappers
  let newsent = runParseMonad  (sequence $ map sequence sentbundle) dm
  print "full count done"
  let counts = M.unionsWith mappend  $ 
               parMap rwhnf (\sents ->  M.unionsWith mappend $ [ M.fromListWith mappend $ 
                   map (\wi -> (pos wi, S.singleton (spine wi))) $ elems sent 
                   | WordInfoSent sent <- sents
                 ] ) newsent
  encodeFile file2 (counts:: SpineExist)



