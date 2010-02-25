{-# LANGUAGE TemplateHaskell, StandaloneDeriving, TypeFamilies, UndecidableInstances, Rank2Types, GeneralizedNewtypeDeriving, FlexibleInstances, TypeSynonymInstances #-}

import NLP.TreeBank.TreeBank
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
import NLP.Model.Dependency.Model
import NLP.Probability.Chain
import NLP.Language.SimpleLanguage
import CountsDep
import NLP.ParseMonad
import NLP.Model.Dependency.Format
import NLP.TreeBank.Label
main = do 
  [file1, label, file2] <- getArgs
  counts <- readAndCount file1 label file2
  print "full count done"
  encodeFile file2 (counts::(Observation FirstOrderDep))

readAndCount file1 label file2 = do
  newsents <- getSentences file1
  print "ending parsing" 
  dm <- loadDebugMappers
  let counts =  parMap rwhnf (countSome dm) $ zip newsents [0..]
  --print counts
  print "count sets done"
  return $! mconcat counts
      where 
        countSome dm (ls,n) = runParseMonad  (do
                             ls' <- sequence (ls:: [ParseMonad WordInfoSent])
                             ctl <- labeler
                             nts <- labels 
                             counts <- mapM  (\wis -> toDependency ctl wis >>= countDep (map convertNTtoLabel nts)) (ls'::[WordInfoSent]) 
                             return $ trace (show n) $ 
                                    mconcat $ map fst counts) dm        
            where (labeler,labels) = pickSelect label
