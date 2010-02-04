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

data WordInfo = 
     WordInfo {
      ind    :: Int, 
      word   :: AWord,
      wordStr :: Word,
      pos    :: APOS,
      posStr :: POS,
      adjoinInd :: Int,
      aspine :: Atom (Spine NonTerm),
      tspine :: Spine (ANonTerm),
      spine  :: Spine (NonTerm),
      adjPos :: Int,
      sister :: AdjunctionType
} deriving (Eq)


--{{{  WordInfo Classes

instance Show (WordInfo) where 
    show wi = 
        intercalate "\t" $ 
        map (\f -> f wi) 
              [show . ind,
               show . wordStr,
               show . posStr,
               show . adjoinInd,
               const "HOLDER",
               show . spine, 
               show . adjPos,
               show . sister
              ]
--}}}

-- parsing Xavier's tree files with spines

parseWordInfo :: Parser (ParseMonad WordInfo)
parseWordInfo =  do 
      n <- nat
      spaces
      word <- parser
      spaces 
      pos <- parser
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
      return $ do
        newword <- collapseWord word 
        aword <- toAtom newword
        apos <- toAtom pos
        aspine <- toAtom spine
        tspine <- TR.mapM toAtom spine 

        return $ WordInfo {
                   ind  = fromIntegral n,
                   wordStr =  newword,
                   word = aword,
                   pos = apos,
                   posStr = pos,
                   adjoinInd = fromIntegral adjInd,
                   tspine = tspine,
                   aspine = aspine,
                   spine = spine, 
                   sister = if sister == 's' then Sister else Regular,
                   adjPos = fromIntegral adjPos
                 }

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
   

testData :: [(String, WordInfo) ]
testData = [(
 "23  in           IN     20  VP+*+PP      *+PP    0  s",
 WordInfo 23 (Atom 2) (mkWord "in") (Atom 1) (mkPOS "IN") 20 (Atom 1) (mkSpine [Atom 2]) (mkSpine [read "PP"])  0 Sister)] 


tests = runTestTT $ TestList [TestLabel "Parsing" test1]

test1 = TestCase $ do 
          mappers <- liftIO $ loadDebugMappers 
          (mapM_ (\(str, testParse) -> 
                      case parse parseWordInfo "" str of
                        Right p -> 
                           assertEqual "parse fail" (runParseMonad  p  mappers) testParse
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
