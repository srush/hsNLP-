{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, ScopedTypeVariables, Rank2Types, ExistentialQuantification, TypeSynonymInstances #-}
module NLP.TreeBank.TreeBank where 

--{{{  Imports
import Data.List
import Helpers.Parse
import Data.Array
import qualified Data.Map as M

import Helpers.Common hiding (char, space)
import Control.Exception
import Data.Maybe (catMaybes)
import Test.HUnit hiding (assert)
import NLP.Grammar.TAG hiding (adjPos)
import NLP.Grammar.Spine
--import qualified NLP.Grammar.NonTerm as NT 
import NLP.Language.SimpleLanguage -- For tests
import Helpers.Arbitrary
import NLP.ParseMonad
import Control.Monad.Trans
import qualified Data.Traversable as TR
--}}}

newtype WordInfoSent = WordInfoSent (Array Int (WordInfo))
    deriving (Eq)


data RawWordInfo = 
    RawWordInfo {
      raw_ind :: Int,
      raw_word :: Word,
      raw_pos :: [POS],
      raw_adjoinInd :: Int,
      raw_spine  :: Spine (NonTerm),
      raw_adjPos :: Int, 
      raw_sister :: AdjunctionType
    } deriving (Eq,Show)

data WordInfo = 
     WordInfo {
      raw :: RawWordInfo,
      word   :: AWord,
      pos    :: [APOS],
      aspine :: Atom (Spine NonTerm),
      tspine :: Spine (ANonTerm)
} deriving (Eq, Show)

ind = raw_ind . raw 
adjoinInd = raw_adjoinInd . raw 
spine  = raw_spine . raw
adjPos = raw_adjPos . raw 
sister = raw_sister . raw
posStr = raw_pos . raw
wordStr = raw_word . raw

--instance UnAtom WordInfo RawWordInfo ParseMonad where 
--    unAtom = 

toWordInfo :: RawWordInfo -> ParseMonad WordInfo
toWordInfo rawWI = do
  newword <- collapseWord $ raw_word rawWI 
  aword <- toAtom newword
  apos <- mapM toAtom $ raw_pos rawWI
  aspine <- toAtom $ raw_spine rawWI 
  tspine <- TR.mapM toAtom $ raw_spine rawWI 
  return $ WordInfo {
                     raw = rawWI,
                     word = aword,
                     pos = apos,
                     aspine = aspine,
                     tspine = tspine
                   }

--{{{  WordInfo Classes

instance Pretty RawWordInfo where 
    pPrint rwi = 
        hcat $ punctuate (text "\t") $ 
        map (\f -> f rwi) 
              [int . raw_ind,
               pPrint . raw_word,
               hcat . punctuate (text "|") . map pPrint . raw_pos,
               pPrint . raw_adjoinInd,
               text . const "HOLDER",
               pPrint . raw_spine, 
               pPrint . raw_adjPos,
               pPrint . raw_sister
              ]

instance Pretty WordInfo where
    pPrint = pPrint . raw
--}}}

-- parsing Xavier's tree files with spines
parseRawWordInfo :: Parser RawWordInfo
parseRawWordInfo = do
      n <- nat
      spaces
      word <- parser
      spaces 
      pos <- parser `sepBy` (char '|') 
      spaces
      adjInd <- nat
      spaces
      manyTill anyChar space
      spaces
      spine <- parseSpine parser
      spaces 
      adjPos <- nat 
      spaces
      sister <- anyChar
      return $ 
             RawWordInfo {
                   raw_ind  = fromIntegral n,
                   raw_word = word,
                   raw_pos = pos,
                   raw_adjoinInd = fromIntegral adjInd,
                   raw_spine = spine, 
                   raw_sister = if sister == 's' then Sister else Regular,
                   raw_adjPos = fromIntegral adjPos
                 }


parseWordInfo :: Parser (ParseMonad WordInfo)
parseWordInfo =  do 
  rawWI <- parseRawWordInfo
  return $ 
         toWordInfo rawWI

-- parseNT :: Parser (NT.NonTermWrap) 
-- parseNT = NT.mkNonTerm `liftM` parser

-- parseSpine :: Parser (Spine (NT.NonTermWrap))
-- parseSpine = do 
--       nonterms <- choice [Just `liftM` parseNT, 
--                           char '*' >> return Nothing] 
--                   `sepBy` char '+'
--       return $ mkSpine $ catMaybes nonterms

parseWordInfoSent :: Parser (ParseMonad WordInfoSent)
parseWordInfoSent = do 
      words <- parseWordInfo `sepEndBy` ((optional $ char ' ') >> newline)
      return $ do 
        ws <- sequence words
        return $ WordInfoSent $ listArray (1,length words) (ws)

instance Show (WordInfoSent) where 
    show (WordInfoSent wis) = intercalate "\n" $ map show $ elems wis

instance Pretty WordInfoSent where 
    pPrint (WordInfoSent wis) =  vcat $ map pPrint $ elems wis
           
 
--parseWordInfo :: String -> Either ParseError (WordInfo) 
--parseWordInfo = parse parser "" 


readSentence :: String -> IO (ParseMonad WordInfoSent)
readSentence file = do
  contents <- readFile file
  case parse parseWordInfoSent file contents of 
    Right s -> return s
    Left error -> throw $ AssertionFailed $ show error 

parseSentences :: String -> String -> ParseMonad [WordInfoSent]
parseSentences file contents = 
  case parse (parseWordInfoSent `sepEndBy` newline)  file contents of 
    Right s -> sequence s
    Left error -> throw $ AssertionFailed $ show error 

readSentences file = do
  contents <- readFile file
  return $ parseSentences file contents

parseSentence :: String -> String -> ParseMonad WordInfoSent
parseSentence file contents = 
  case parse parseWordInfoSent file contents of 
    Right s ->  s
    Left error -> throw $ AssertionFailed $ show error 



--{{{  TESTS
   

testData :: [(String, RawWordInfo) ]
testData = [(
 "23  in           IN     20  VP+*+PP      *+PP    0  s",
 RawWordInfo 23 (mkWord "in") [mkPOS "IN"] 20 (mkSpine [mkNonTerm "PP"])  0 Sister),
            ("23  in           IN|RB     20  VP+*+PP      *+PP    0  s",
             RawWordInfo 23 (mkWord "in") [mkPOS "IN",mkPOS "RB"] 20 (mkSpine [mkNonTerm "PP"])  0 Sister)
 ] 



tests = runTestTT $ TestList [TestLabel "Parsing" test1]

test1 = TestCase $ do 
          mappers <- liftIO $ loadDebugMappers 
          (mapM_ (\(str, testParse) -> 
                      case parse parseRawWordInfo "" str of
                        Right p -> 
                           assertEqual "parse fail" p testParse
                        Left mes ->
                           liftIO $ print mes
                 ) 
                  testData)

-- prop_showParse w =  case parse parser "" (show w) of 
--                      Right s -> w == s
--                      Left error ->  False --throw $ AssertionFailed $ show error 
--     where types = (w::WordInfo ) 

-- prop_showParseSent w = case parse parser "" (show w) of 
--                      Right s -> w == s
--                      Left error ->  False --throw $ AssertionFailed $ show error 
--     where types = (w::WordInfoSent) 

--instance (Language l) => Arbitrary (WordInfoSent l) where 
--    arbitrary = nonEmptyArray WordInfoSent

--instance (Language l) => Arbitrary (WordInfo l) where 
--    arbitrary = return WordInfo `ap` positive `ap` arbitrary `ap` arbitrary `ap` arbitrary `ap` 
--                positive `ap` arbitrary `ap` positive `ap` arbitrary 

--}}}
