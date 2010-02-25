module NLP.Controller where 

import Helpers.Common
import NLP.CNF.CKY
import NLP.Format.TreeBank
import NLP.CNF

makeWordTable sentences = 
  mkWordTable $ concatMap fst sentences

  

readParams = do 
  params <- readInsideParams
  return params 

readTestData testfile = do 
  sentences <- readSentenceBunch testfile
  let obs = map fst sentences
  return obs

evalTestData testSentences params =
    map (splitDer . simpleViterbi params) testSentences 
  