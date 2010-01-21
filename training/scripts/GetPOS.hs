import TreeBank 
import TAG 
import TAGparse
import System (getArgs) 
import System.IO
import Data.Monoid
import Data.Binary
import Data.List
import Data.Array
import qualified Data.Map as M
import qualified Data.Set as S
import NLP.ChartParse.Eisner hiding (word)
import NLP.ChartParse
import NLP.Semiring.Derivation
import Control.Exception
import Control.Parallel.Strategies
import DataHelpers
import Prior
import Sentence

main = do 
  [file1] <- getArgs
  sentbundle <- getSentences file1
  let posi = mconcat  $ 
               parMap rwhnf (\sents ->  mconcat $ [ mconcat $ 
                   map (S.singleton . pos) (elems sent) 
                   | WordInfoSent sent <- sents
                 ]) sentbundle
  putStrLn $ show $ S.toList posi
