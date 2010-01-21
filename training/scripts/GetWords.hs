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
import qualified Data.Trie as T
import qualified Data.IntMap as IM
import Data.Binary
import qualified Data.ByteString.Char8 as BS
main = do 
  [file1] <- getArgs
  sentbundle <- getSentences file1
  let words = mconcat  $ 
                  [ mconcat $ map (S.singleton . word)  $ elems sent 
                   | WordInfoSent sent <-  concat sentbundle]
  let trie = T.fromList $ zip (map (BS.pack .show) $ S.toList words) [(1::Int)..]           
  let intmap = IM.fromList $ zip [(1::Int)..] (map (show) $ S.toList words) 
  
  encodeFile "/tmp/allwords" $ (trie, intmap)
      