import PreProcess
import NLP.TreeBank.TreeBank
import System (getArgs) 
import System.IO
import Helpers.Common
import DataHelpers
import NLP.Language.SimpleLanguage
import NLP.ParseMonad
main = do 
  [file1] <- getArgs
  cont <- readFile file1
  sentbundle <- getSentences file1
  dm <- loadDebugMappers
  let (isComma', isPunc') = runParseMonad (do {c<-isComma;p<- isPunc;return (c,p)}) dm
      np = read "NP" 
      npb = read "NPB"
  sequence_ $ 
         map (\sents -> do  
                  putStrLn $ intercalate "\n\n" $ 
                           map (show . toWIS . augmentNP np npb . reapply 20 (liftNode (isComma' . pos)) . reapply 5 (removeFront (isComma' . pos)) . reapply 5 (removeEnd (isComma' . pos)) .  filterNode (isPunc' . pos) . fromWIS . (\s -> runParseMonad s dm)) sents
                  putStrLn ""
   ) sentbundle
  where
    reapply 0 _ d = d 
    reapply n f d = reapply (n-1) f $ f d 
