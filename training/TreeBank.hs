{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TreeBank where 
import Data.List

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
--import Data.Array
import Data.Array.IArray
import qualified Data.Map as M
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)
import Control.Monad (liftM, ap)
import Control.Exception
import Data.Either.Unwrap (fromRight)
import Data.Maybe (catMaybes)
import Test.HUnit
import Test.QuickCheck
import Sentence 
import ArbitraryHelpers
import TAG
import NLP.Semiring
import Dependency
import DependencyStructure
import Debug.Trace

import qualified NLP.ChartParse as CP


newtype WordInfoSent = WordInfoSent (Array Int WordInfo)
    deriving (Eq)

data WordInfo = WordInfo {
      ind    :: Int,
      word   :: Word,
      pos    :: POS,
      adjoinInd :: Int,
      spine  :: Spine,
      adjPos :: Int,
      sister :: AdjunctionType
} deriving (Eq)



instance Arbitrary WordInfo where 
    arbitrary = return WordInfo `ap` positive `ap` arbitrary `ap` arbitrary `ap` 
                positive `ap` arbitrary `ap` positive `ap` arbitrary

instance Show WordInfo where 
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

-- parsing Xavier's tree files with spines

lexer       = P.makeTokenParser haskellDef    
nat      = P.natural lexer

class Parsable a where 
    parser :: Parser a

instance Parsable WordInfo where 
    parser = do 
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
      spine <- parser
      spaces 
      adjPos <- nat 
      spaces
      sister <- anyChar
--      if adjPos /= 0 && adjInd == 0 then throw $ AssertionFailed (show word ++ " "  ++ show n ++ show adjInd ++ show adjPos)
      return $ WordInfo {
                   ind = fromIntegral n,
                   word = word,
                   pos = pos,
                   adjoinInd = fromIntegral adjInd,
                   spine = spine, 
                   sister = if sister == 's' then Sister else Regular,
                   adjPos = fromIntegral adjPos
                 }

parseString = manyTill anyChar space

instance Parsable Word where 
    parser = Word `liftM` parseString 

instance Parsable POS where 
    parser = POS `liftM` parseString 

instance Parsable NonTerm where 
    parser = NonTerm `liftM` (many1 $ choice [upper, char '_'])

instance Parsable Spine where 
    parser = do 
      nonterms <- choice [Just `liftM` parser, 
                          char '*' >> return Nothing] 
                  `sepBy` char '+'
      return $ Spine $ catMaybes nonterms

instance Parsable WordInfoSent where 
    parser = do 
      words <- parser `sepEndBy` ((optional $ char ' ') >> newline)
      return $ WordInfoSent $ listArray (1,length words) words

instance Show WordInfoSent where 
    show (WordInfoSent wis) = intercalate "\n" $ map show $ elems wis
           
instance Arbitrary WordInfoSent where 
    arbitrary = nonEmptyArray WordInfoSent
 
parseWordInfo :: String -> Either ParseError WordInfo 
parseWordInfo = parse parser "" 


readSentence :: String -> IO WordInfoSent
readSentence file = do
  contents <- readFile file
  case parse parser file contents of 
    Right s -> return s
    Left error -> throw $ AssertionFailed $ show error 

parseSentences :: String -> String -> [WordInfoSent]
parseSentences file contents = 
  case parse (parser `sepEndBy` newline)  file contents of 
    Right s -> s
    Left error -> throw $ AssertionFailed $ show error 

parseSentence :: String -> String -> WordInfoSent
parseSentence file contents = 
  case parse parser file contents of 
    Right s -> s
    Left error -> throw $ AssertionFailed $ show error 

    
toSentence :: (Semiring semi) => WordInfoSent -> Sentence semi GWord
toSentence (WordInfoSent wis)=  
    mkSimpleSentence $ map (\wi -> (word wi, pos wi)) $ elems wis


toDependency :: (Semiring semi) => WordInfoSent -> DependencySentence semi
toDependency (WordInfoSent wis) = DependencySentence sent depstruct
    where sent = toSentence (WordInfoSent wis)
          depstruct = Dependency $ M.fromList $ map convertWI  $ elems wis

          -- HACK: using the Eisner notation word_n+1 is the ROOT symbol
          -- change the 0 notation from the treebank to n+1
          convertWI wi = (ind wi, if head == 0 then DEdge (n + 1) () else DEdge head ())
              where n = slength sent
                    head = adjoinInd wi


--toTagSentence :: WordInfoSent -> Sentence TAGCountSemi (GWord, Spine)
toTagSentence initSemi (WordInfoSent wis)=  
    mkTagWords initSemi $  map (\wi -> (((word wi, pos wi), spine wi))) $ elems wis


--toTAGDependency :: WordInfoSent -> TAGSentence TAGCountSemi
toTAGDependency initSemi (WordInfoSent wis) = TAGSentence sent depstruct
    where sent = toTagSentence initSemi (WordInfoSent wis)
          depstruct = Dependency $ M.fromList $ map convertWI  $ elems wis

          convertWI wi = (ind wi, if head == 0 then DEdge (n + 1) $ (adjPos wi, sister wi) else DEdge head $ (adjPos wi, sister wi))
              where n = slength sent
                    head = adjoinInd wi

--testGraphViz = do 
--  sent <- readSentence "data/sample.data"
--  let dsent = toDependency sent
--  showDotGraph $ convertToGraph dsent
-- tests 


testData = [(
 "23  in           IN     20  VP+*+PP      *+PP    0  s",
 WordInfo 23 (Word "in") (POS "IN") 20 (Spine [NonTerm "PP"]) 0 Sister)] 


tests = runTestTT $ TestList [TestLabel "Parsing" test1]

test1 = TestCase (mapM_ (\(str, testParse) -> 
                         assertEqual "parse fail" (fromRight $ parse parser "" str) testParse) 
                  testData)

prop_showParse w = case parse parser "" (show w) of 
                     Right s -> w == s
                     Left error ->  False --throw $ AssertionFailed $ show error 
    where types = (w::WordInfo) 

prop_showParseSent w = case parse parser "" (show w) of 
                     Right s -> w == s
                     Left error ->  False --throw $ AssertionFailed $ show error 
    where types = (w::WordInfoSent) 
