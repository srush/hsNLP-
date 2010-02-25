import NLP.TreeBank.Dependency

import System.IO
import System (getArgs) 
main = do
    [infile] <- getArgs 
    a <- convertFilePOS infile 
    putStrLn a                                                                                                                
