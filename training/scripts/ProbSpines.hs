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
import NLP.Language.SimpleLanguage
import NLP.Model.TAG.Prior
import NLP.Grammar.TAG
import NLP.Model.TAG.Wrap
import NLP.Probability.Chain
import NLP.ParseMonad
main = do 
  [file1, file2] <- getArgs
  sentbundle <- getSentences file1
  dm <- loadDebugMappers
  let newsent = runParseMonad (sequence $ map sequence sentbundle) dm
  let myroot = runParseMonad (root 0) dm
  print "full count done"
  let counts = mconcat  $ 
               parMap rwhnf (\sents ->  mconcat $ [ countPrior $  
                   (myroot: (map (\wi -> mkTAGWord  (GWord (word wi, pos wi), aspine wi) (tspine wi) 0 ) $ elems sent)) 
                   | WordInfoSent sent <- sents
                 ] ) newsent
  encodeFile file2 (counts::Observation (CollinsPrior))
