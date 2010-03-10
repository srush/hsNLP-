import NLP.Model.TAG.Adjunction
import NLP.Probability.Chain
import System (getArgs) 
import System.IO
import Data.Binary
import Data.Monoid

main = do 
  args <- getArgs
  counts <- mapM decodeFile $ tail (args::[String])
  encodeFile (head args) (mconcat counts::ChainedObs (Collins))