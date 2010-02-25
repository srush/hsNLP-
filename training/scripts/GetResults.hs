import System
import NLP.Decomp.Results

main = do 
  [resultFile, t] <- getArgs
  contents <- readFile resultFile
  let results = getResults contents
  let fn = case t of 
             "t" -> testStr
             "g" -> goldStr
  mapM_ putStrLn  $ map fn results