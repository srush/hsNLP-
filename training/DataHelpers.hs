module DataHelpers where 

import NLP.TreeBank.TreeBank 
import Data.List

ngroup [] n = []
ngroup ls n = 
    (take n ls) : (ngroup (drop n ls) n)   

separate :: (Eq el) => el -> [el] -> [[el]]
separate el [] = [] 
separate el ls = case elemIndex el ls of
                   Just n -> 
                       (take n ls): (separate el (drop (n+1) ls))
                   Nothing -> []

getSentences file1 = do 
  contents <- readFile file1
  let sents =  separate "" $ lines contents
  let nsents = ngroup sents 1000 
  return $ map (map (parseSentence file1. unlines)) nsents   
