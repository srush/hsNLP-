
import qualified Data.Map as M
import System (getArgs)
import Data.Char (ord)
getWord l = words l !! 1
putWord l word = unwords ((ws !! 0) : word: drop 2 ws)  
    where ws = words l  

isNum a = ord a >= ord '0' && ord a <= ord '9' 

main = do
  [file] <- getArgs
  contents <- readFile file
  let counts = M.fromListWith (+) $ map (\l -> (getWord l, 1)) $ filter (/= "") $ lines contents
  let newCounts = M.filter (> 5) counts
  putStrLn $ unlines $ M.keys newCounts