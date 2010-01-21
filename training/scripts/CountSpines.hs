import NLP.TreeBank.TreeBank 
import NLP.TreeBank.TAG
import NLP.Model.TAGparse
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
import NLP.Language.English
main = do 
  [file1, file2] <- getArgs
  sentbundle <- getSentences file1
  print "full count done"
  let counts = M.unionsWith mappend  $ 
               parMap rwhnf (\sents ->  M.unionsWith mappend $ [ M.fromListWith mappend $ 
                   map (\wi -> (pos wi, S.singleton (spine wi))) $ elems sent 
                   | WordInfoSent sent <- sents
                 ] ) sentbundle
  encodeFile file2 (counts::SpineExist English)



