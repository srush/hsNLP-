import Helpers.Common
import Data.List 
import NLP.Model.TAG.Decoding
import NLP.Language.SimpleLanguage
import System.IO
import System
import NLP.TreeBank.TreeBank
import NLP.ParseMonad
import NLP.Model.TAG.DependencyPrior
import NLP.ParseMonad
import Data.Array
main = do 
  [file1] <- getArgs
  contents <- readFile file1
  let sentsM = parseSentences file1 contents
  mappers <- loadMappers defaultLoadMap { shouldCollapseWords = False}
  let sents = runParseMonad sentsM mappers  

  putStrLn $ intercalate "\n" $ map (\(WordInfoSent wis) -> intercalate " "$  map (\wi -> (show $ wordStr wi)++ "/" ++(show $ posStr wi)) $ elems wis) sents

