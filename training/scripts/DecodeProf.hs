{-# LANGUAGE BangPatterns #-}
import Helpers.Common
import Data.List 
import NLP.Model.TAG.Decoding
import NLP.Language.SimpleLanguage
import System.IO
import System
import NLP.TreeBank.TreeBank
import NLP.ParseMonad
import NLP.Model.TAG.DependencyPrior

main = do 
  [testFile,pruneFile] <- getArgs
  --params <- (readDecodeParams adjCountFile spineCountFile spineProbFile) :: IO (DecodeParams English)
  params <- basicParams
  dm <- loadDebugMappers 
  --print $ params `seq` "Params Loaded"
  sents <- readSentences testFile
  let newsents = runParseMonad sents dm
  --print $ sents `seq` "Sentences loaded"
  prunes <- readPruning pruneFile
  --print $ show newsents
  mapM_ (\(s,p) -> do 
            runParseMonad (do 
                            tm <- tripletMapper
                            Just b <- decodeGold params s 
                            Just b' <- genDecodeSentence (defaultDecoding{validator=validByDepPrior tm p}) params s
                            renderSentences b b'
                            --return $ print "hello"
                            ) dm) $ zip (take 20 newsents) prunes

