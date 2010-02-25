import PreProcess
import NLP.TreeBank.TreeBank
import System (getArgs) 
import System.IO
import Helpers.Common
import DataHelpers
import NLP.Language.SimpleLanguage
import NLP.ParseMonad
import Data.Array 

main = do 
  [file1] <- getArgs
  cont <- readFile file1
  sentbundle <- getSentences file1
  dm <- loadMappers $ defaultLoadMap {shouldCollapseWords = False}
  let (isComma', isPunc') = runParseMonad (do {c<-isComma;p<- isPunc;return (c,p)}) dm

  
  let processed = map (\sents -> map ( toWIS . reapply 20 (liftNode (isComma' . pos)) . reapply 5 (removeFront (isComma' . pos)) . reapply 5 (removeEnd (isComma' . pos)) .  filterNode (isPunc' . pos) . fromWIS . (\s -> runParseMonad s dm)) sents) sentbundle

  let both = zip (concat processed) (concat (runParseMonad (sequence (map sequence sentbundle)) dm))

  let words = map (\(WordInfoSent proc, WordInfoSent orig) -> align (elems proc) (elems orig)) both
  putStrLn $ show words

  where
    reapply 0 _ d = d 
    reapply n f d = reapply (n-1) f $ f d 

    align [] orig  = [] 
    align fullproc@(procWord:proc) (origWord:orig) = 
        if wordStr procWord == wordStr origWord then  
            (ind procWord, ind origWord): align proc orig
        else 
            align fullproc orig