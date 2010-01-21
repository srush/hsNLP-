import NLP.Model.Adjunction
import NLP.Model.Chain
import System (getArgs) 
import System.IO
import Data.Binary
import Data.Monoid
import NLP.Language.English
main = do 
  args <- getArgs
  counts <- mapM decodeFile $ tail (args::[String])
  encodeFile (head args) (mconcat counts::Observation (Collins English))