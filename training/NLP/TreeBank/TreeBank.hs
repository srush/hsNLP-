{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, ScopedTypeVariables, Rank2Types, ExistentialQuantification, TypeSynonymInstances #-}
module NLP.TreeBank.TreeBank where 


--{{{  Imports
import Data.List
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Data.Array
import qualified Data.Map as M
import qualified Text.Parsec.Token as P
import Helpers.Common hiding (char, space)
import Text.Parsec.Language (haskellDef)
import Control.Exception
import Data.Either.Unwrap (fromRight)
import Data.Maybe (catMaybes)
import Test.HUnit hiding (assert)
import NLP.Grammar.TAG hiding (adjPos)
import NLP.Grammar.Spine
import qualified NLP.Grammar.NonTerm as NT 
import NLP.Language
import NLP.Language.English -- For tests
import Helpers.Arbitrary
--}}}

newtype WordInfoSent l = WordInfoSent (Array Int (WordInfo l))
    deriving (Eq)

data WordInfo l = Language l => 
     WordInfo {
      ind    :: Int, 
      word   :: Word l,
      wordStr :: String,
      pos    :: POS l,
      adjoinInd :: Int,
      spine  :: Spine (NT.NonTermWrap l),
      adjPos :: Int,
      sister :: AdjunctionType
} 


--{{{  WordInfo Classes

instance (Language l) => Eq (WordInfo l) 

instance (Language l) => Show (WordInfo l) where 
    show wi = 
        intercalate "\t" $ 
        map (\f -> f wi) 
              [show . ind,
               show . word,
               show . pos,
               show . adjoinInd,
               const "HOLDER",
               show . spine, 
               show . adjPos,
               show . sister
              ]
--}}}

-- parsing Xavier's tree files with spines


lexer = P.makeTokenParser haskellDef    
nat  = P.natural lexer
parseString = manyTill anyChar space

class Parsable a where 
    parser :: Parser a

instance (Language l) => Parsable (WordInfo l) where 
    parser =  do 
      n <- nat
      spaces
      word <- parseString 
      spaces 
      pos <- parseString
      spaces
      adjInd <- nat
      spaces
      manyTill anyChar space
      spaces
      spine <- parseSpine
      spaces 
      adjPos <- nat 
      spaces
      sister <- anyChar
      return $ WordInfo {
                   ind  = fromIntegral n,
                   word = mkWord (word::String),
                   wordStr = word,
                   pos  = mkPOS pos,
                   adjoinInd = fromIntegral adjInd,
                   spine = spine, 
                   sister = if sister == 's' then Sister else Regular,
                   adjPos = fromIntegral adjPos
                 }

parsePOS :: (Language l) => Parser (POS l) 
parsePOS = mkPOS `liftM` parseString 

parseNT :: (Language l ) => Parser (NT.NonTermWrap l) 
parseNT = NT.mkNonTerm `liftM` (many1 $ choice [upper, char '_'])

parseSpine :: (Language l) => Parser (Spine (NT.NonTermWrap l))
parseSpine = do 
      nonterms <- choice [Just `liftM` parseNT, 
                          char '*' >> return Nothing] 
                  `sepBy` char '+'
      return $ mkSpine $ catMaybes nonterms

instance (Language l) => Parsable (WordInfoSent l) where 
    parser = do 
      words <- parser `sepEndBy` ((optional $ char ' ') >> newline)
      return $ WordInfoSent $ listArray (1,length words) words

instance (Language l) => Show (WordInfoSent l) where 
    show (WordInfoSent wis) = intercalate "\n" $ map show $ elems wis
           
 
parseWordInfo :: (Language l) => String -> Either ParseError (WordInfo l) 
parseWordInfo = parse parser "" 


readSentence :: (Language l) => String -> IO (WordInfoSent l)
readSentence file = do
  contents <- readFile file
  case parse parser file contents of 
    Right s -> return s
    Left error -> throw $ AssertionFailed $ show error 

parseSentences :: (Language l) => String -> String -> [WordInfoSent l]
parseSentences file contents = 
  case parse (parser `sepEndBy` newline)  file contents of 
    Right s -> s
    Left error -> throw $ AssertionFailed $ show error 

parseSentence :: (Language l) => String -> String -> WordInfoSent l
parseSentence file contents = 
  case parse parser file contents of 
    Right s ->  s
    Left error -> throw $ AssertionFailed $ show error 

--{{{  TESTS
   

testData :: [(String, WordInfo English) ]
testData = [(
 "23  in           IN     20  VP+*+PP      *+PP    0  s",
 WordInfo 23 (mkWord "in") "in" (mkPOS "IN") 20 (mkSpine [NT.mkNonTerm "PP"]) 0 Sister)] 


tests = runTestTT $ TestList [TestLabel "Parsing" test1]

test1 = TestCase (mapM_ (\(str, testParse) -> 
                         assertEqual "parse fail" (fromRight $ parse parser "" str) testParse) 
                  testData)

prop_showParse w =  case parse parser "" (show w) of 
                     Right s -> w == s
                     Left error ->  False --throw $ AssertionFailed $ show error 
    where types = (w::WordInfo English) 

prop_showParseSent w = case parse parser "" (show w) of 
                     Right s -> w == s
                     Left error ->  False --throw $ AssertionFailed $ show error 
    where types = (w::WordInfoSent English) 

--instance (Language l) => Arbitrary (WordInfoSent l) where 
--    arbitrary = nonEmptyArray WordInfoSent

--instance (Language l) => Arbitrary (WordInfo l) where 
--    arbitrary = return WordInfo `ap` positive `ap` arbitrary `ap` arbitrary `ap` arbitrary `ap` 
--                positive `ap` arbitrary `ap` positive `ap` arbitrary 

--}}}
