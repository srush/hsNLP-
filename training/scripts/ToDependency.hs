import NLP.TreeBank.Dependency

import System.IO
import System (getArgs) 
main = do
    [infile, format] <- getArgs 
    case format of 
      "mapped" -> do
          a <- convertFile infile  "../terry/data/ptb/ftags.map" "../terry/data/ptb/ctags.map" "../terry/data/ptb/words.map"
          putStrLn a                                                                                                                
      "gold" -> do
          a <- convertGoldFile infile
          putStrLn a                                                                                                                