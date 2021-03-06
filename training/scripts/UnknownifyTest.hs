import qualified Data.Map as M
import System (getArgs)
import Data.Char (ord)
getWord l = words l !! 1
putWord l word = unwords ((ws !! 0) : word: drop 2 ws)  
    where ws = words l  

isNum a = ord a >= ord '0' && ord a <= ord '9' 

main = do
  [train, test] <- getArgs
  contents <- readFile train
  let counts = M.fromListWith (+) $ map (\l -> (getWord l, 1)) $ filter (/= "") $ lines contents
  contentsTest <- readFile test
  putStr $ unlines $ 
         map (\l -> if l == "" then l 
                    else if (isNum (getWord l !! 0)) then putWord l "*NUM*"
                    else if ((getWord l) == "--") then putWord l "*DDASH*"
                    else if (M.findWithDefault 0 (getWord l) counts) > 5 then l 
                     else putWord l "*UNK*" ) $ lines contentsTest
  