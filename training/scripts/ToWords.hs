import NLP.TreeBank.Dependency

import System.IO
import System (getArgs) 
main = do
    [infile] <- getArgs 
    a <- convertFileWord infile 
    putStrLn a                                                                                                                
