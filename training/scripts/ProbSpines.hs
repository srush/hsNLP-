import NLP.TreeBank.TreeBank 
import NLP.TreeBank.TAG
import NLP.Model.TAGparse hiding (word)
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
import NLP.Model.ChainPrior
import NLP.Grammar.TAG
import NLP.Language
import NLP.Model.TAGWrap
import NLP.Model.Chain

main = do 
  [file1, file2] <- getArgs
  sentbundle <- getSentences file1
  print "full count done"
  let counts = mconcat  $ 
               parMap rwhnf (\sents ->  mconcat $ [ mconcat $ 
                   (simpleObserve (PrEv (root 0)) (PrCon ()) : (map (\wi -> simpleObserve (PrEv $ mkTAGWord  (GWord (word wi, pos wi)) (spine wi) 0 ) $ PrCon ()) $ elems sent)) 
                   | WordInfoSent sent <- sents
                 ] ) sentbundle
  encodeFile file2 (counts::Observation (CollinsPrior English))
