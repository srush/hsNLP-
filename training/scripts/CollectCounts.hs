{-# LANGUAGE TemplateHaskell, StandaloneDeriving, TypeFamilies, UndecidableInstances, Rank2Types, GeneralizedNewtypeDeriving, FlexibleInstances, TypeSynonymInstances #-}
import NLP.Model.TAGparse
import NLP.TreeBank.TAG
import System (getArgs) 
import System.IO
import Data.Monoid
import Data.Binary
import Data.List
import NLP.ChartParse.Eisner.Inside
import NLP.Semiring.Derivation
import Control.Exception
import Control.Parallel.Strategies
import DataHelpers
import Debug.Trace
import NLP.Model.Adjunction
import NLP.Model.Chain
import NLP.Language.English
import Counts 

main = do 
  [file1, file2] <- getArgs
  counts <- readAndCount file1 file2
  print "full count done"
  encodeFile file2 (counts::(Observation (Collins English)))

readAndCount file1 file2 = do
  newsents <- getSentences file1
  print "ending parsing" 
  let counts =  parMap rwhnf countSome $ zip newsents [0..]
  --print counts
  print "count sets done"
  return $! mconcat counts
      where 
        countSome (ls,n) =  
          --return $! 
          trace (show n) $ 
                mconcat $ map (fst. countTAG . toTAGDependency) ls 
        
            
