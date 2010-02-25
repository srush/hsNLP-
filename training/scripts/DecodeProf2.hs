{-# LANGUAGE BangPatterns #-}
import Helpers.Common
import Data.List 
import NLP.Model.TAG.Decoding
import NLP.Language.SimpleLanguage
import System.IO
import System
import NLP.TreeBank.TreeBank
import NLP.ParseMonad
separate :: (Eq el) => el -> [el] -> [[el]]
separate el [] = [] 
separate el ls = case elemIndex el ls of
                   Just n -> 
                       (take n ls): (separate el (drop (n+1) ls))
                   Nothing -> []

main = do 
  [testFile] <- getArgs
  --params <- (readDecodeParams adjCountFile spineCountFile spineProbFile) :: IO (DecodeParams English)
  (!params) <- basicParams
  dm <- loadDebugMappers 
  print $ params `seq` "Params Loaded"
  (!sents) <- readSentences testFile
  let newsents = runParseMonad sents dm
  print $ sents `seq` "Sentences loaded"

  mapM_ (\s -> do 
           runParseMonad (do 
                           Just b <- decodeGold params s 
                           Just b' <- decodeSentence params s
                           renderSentences b b'           
                           ) dm) $ take 5 newsents

