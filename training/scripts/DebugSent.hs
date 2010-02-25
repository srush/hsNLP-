{-# LANGUAGE OverloadedStrings #-}
import Helpers.Common
import Data.List 
import NLP.Model.TAG.Decoding
import NLP.Language.SimpleLanguage
import System.IO
import System
import NLP.TreeBank.TreeBank
import NLP.ParseMonad
import NLP.Model.TAG.DependencyPrior
import NLP.Model.TAG.Adjunction
import NLP.ParseMonad
import Debug.Trace
import NLP.Grammar.TAG hiding (adjType)
import NLP.Model.CreateableSemi
import NLP.Model.TAG.Semi
import qualified Data.Text as T
import qualified Data.Map as M
main = do 
  [testFile, fixedDependFile] <- getArgs
  params <- basicParams
  contents <- readFile testFile
  mappers <- loadDebugMappers 
  hSetBuffering stdout NoBuffering
  let sentsM =  mapM (parseSentence testFile. unlines) $ 
               separate "" $ lines contents
  let sents = runParseMonad sentsM mappers  

  fixedContents <- readFile fixedDependFile
  let fixie = M.fromList $ map ((\ [a,b] -> (read $ T.unpack a, read $ T.unpack b)) . T.split " " . T.pack) $ lines fixedContents
  let parses =  mapM (\s -> do
                        Just b <- (genDecodeSentence (defaultDecoding 
                                                 {listPruning = False, beamThres = 1e5} ) params s):: ParseMonad (Maybe (CVDDebug Debug TAGDerivation)) 
                                                                                            
                                  
                        b' <- (genDecodeSentence (defaultDecoding 
                                                 {listPruning = False, beamThres = 1e5, 
                                                  validator= const $ \ e c -> case childInd e of 
                                                                                Just cind -> maybe True (\i ->(i == (parentInd c)) && 
                                                                                                              (adjType e == Just Sister)
                                                                                                        ) $ 
                                                                                              M.lookup cind fixie 
                                                                                Nothing -> True
                                                 } ) params s  ):: ParseMonad (Maybe (CVDDebug Debug TAGDerivation)) 
                        return (b', b)) $ sents 
  let results = [(b', b)
                      |  (Just b', b) <- runParseMonad parses mappers]
  
  
  -- putStrLn $ chartStats $ snd $ head results

  sequence $ runParseMonad (mapM (\(b,a) ->  renderSentences a b ) results) mappers 
  sequence $ runParseMonad (mapM (\(b,a) ->  renderDebug a b ) results) mappers 
      where isNothing Nothing = True
            isNothing _ = False
