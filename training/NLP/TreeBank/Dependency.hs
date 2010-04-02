{-# LANGUAGE OverloadedStrings #-}

module NLP.TreeBank.Dependency where 

import qualified Data.Text as T
import qualified Data.Bimap as B
import NLP.TreeBank.TreeBank
import Helpers.Common
import Data.Array
import NLP.Language.SimpleLanguage
import NLP.ParseMonad hiding (readMapped)
import Safe 
import NLP.Grammar.Spine
import qualified Data.Traversable as T
type SimpleDependency = [(Int,Int)]

readDependency :: String -> IO SimpleDependency
readDependency file = do
    contents <- readFile file
    let ls = filter (not .T.null) $ T.lines $ T.pack contents
    let depls = map (readNote "dependency read fail" .T.unpack) $ map (\ a -> a !! 2 ) $  map (T.split " ") ls 
    
    return $ zip [1..] depls

readMapped :: String -> IO (B.Bimap Int String)
readMapped file = do 
  contents <- readFile file
  let all = map (T.split " ") $ T.lines $ T.pack contents
  return $ B.fromList $ map (\[a,b] -> (read $ T.unpack a, T.unpack b)) all

type POSMap = B.Bimap Int String

type WordMap = B.Bimap Int String

readWordMap :: String -> IO WordMap
readWordMap = readMapped  

readPOSMap :: String -> IO POSMap
readPOSMap = readMapped  

-- 1 1116 _ 9 10 _ 4 1
-- Format is ( index word _ ctag ftag _ dep label)  
convertToDependency ::   WordMap -> POSMap -> POSMap -> WordInfoSent -> Doc
convertToDependency   wordMap  posMap ctagMap (WordInfoSent wis) = 
    vcat $ map (\wi -> 
                (int $ ind wi) <+> 
                (int $ fromJustNote "word fail "$ B.lookupR (show $ wordStr wi) wordMap) <+>  
                (text "_") <+>
                (int $ fromJustNote "pos fail" $ B.lookupR (take 2 $ show $ posStr wi) ctagMap) <+>
                (int $ fromJustNote "pos fail" $ B.lookupR (show $ posStr wi) posMap) <+>

                (text "_") <+>
                (int $ adjoinInd wi) <+> 


                (int 0)
                ) $ elems wis

-- format is 1	Influential	_	JJ	JJ	_	2	NMOD
convertToGold :: WordInfoSent -> Doc
convertToGold  (WordInfoSent wis) = 
    vcat $ map (\wi -> 
                (int $ ind wi) <+> 
                (text (show $ wordStr wi) ) <+>  
                (text "_") <+>
                (text  (take 2 $ show $ posStr wi)) <+>
                (text (show $ posStr wi)) <+>

                (text "_") <+>
                (int $ adjoinInd wi) <+> 
                (text "ROOT")
                ) $ elems wis

convertFile :: String -> String -> String -> String-> IO String
convertFile sentFile posMapFile ctagFile wordMapFile = do
   posMap <- readPOSMap posMapFile
   ctagMap <- readPOSMap ctagFile
   wordMap <- readWordMap wordMapFile
   dm <- loadMappers $ defaultLoadMap{shouldCollapseWords = False} 
   sents <- readSentences sentFile
   return $ render $ vcat $ punctuate (text "\n") $ map (convertToDependency wordMap posMap ctagMap) (runParseMonad sents dm)

convertGoldFile :: String -> IO String
convertGoldFile sentFile = do
   dm <- loadMappers $ defaultLoadMap{shouldCollapseWords = False} 
   sents <- readSentences sentFile
   return $ render $ vcat $ punctuate (text "\n") $ map (convertToGold) (runParseMonad sents dm)

convertToSpine :: WordInfoSent -> Doc
convertToSpine (WordInfoSent wis) = 
    vcat $ map (\wi -> 
                (pPrint $  wordStr wi) <+>  
                (pPrint $  posStr wi) <+>  
                --(text "O")
                (((\a -> if isEmpty a then text "O" else text "B-" <> a) $ hcat $  punctuate (text "-") $ map pPrint $ toList $ spine wi))
                ) $ elems wis


convertToPOS :: WordInfoSent -> Doc
convertToPOS (WordInfoSent wis) = 
    vcat $ map (\wi -> 
                (pPrint $  wordStr wi) <+>  
                --(text "O")
                (pPrint $ head $  posStr wi)
                ) $ elems wis



convertToWords :: WordInfoSent -> Doc
convertToWords (WordInfoSent wis) = 
    hcat $ punctuate space  $ map (\wi -> 
                (pPrint $  wordStr wi)) $ elems wis


convertFileSpine :: String -> IO String
convertFileSpine sentFile = do
   dm <- loadMappers $ defaultLoadMap{shouldCollapseWords = False} 
   sents <- readSentences sentFile
   return $ render $ vcat $ punctuate (text "\n") $ map (convertToSpine) (runParseMonad sents dm)

convertFilePOS :: String -> IO String
convertFilePOS sentFile = do
   dm <- loadMappers $ defaultLoadMap{shouldCollapseWords = False} 
   sents <- readSentences sentFile
   return $ render $ vcat $ punctuate (text "\n") $ map (convertToPOS) (runParseMonad sents dm)

convertFileWord :: String -> IO String
convertFileWord sentFile = do
   dm <- loadMappers $ defaultLoadMap{shouldCollapseWords = False} 
   sents <- readSentences sentFile
   return $ render $ vcat  $ map (convertToWords) (runParseMonad sents dm)