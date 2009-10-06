{-# LANGUAGE TypeFamilies, TypeSynonymInstances, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Sentence where 
import NLP.ChartParse 
import Data.Array
import Test.QuickCheck
import Control.Monad (liftM, ap)
import Data.Either.Unwrap (fromRight)
import Data.Char (toUpper)
import Debug.Trace
import ArbitraryHelpers
import Data.DeriveTH
import Data.Binary hiding (Word)
import qualified Data.ByteString.Char8 as BS
import POS 
import Word
import EnumHelpers
import Control.Exception
--import StringTable.Atom

type GWord = (Word, POS)

instance Enum GWord where 
    fromEnum (a,b) = mkFromEnum2 (a, maxBound) (b, maxBound)
    toEnum = mkToEnum2 (maxBound, maxBound)

-- ROOT is a special symbol that we put at the end of the sentence 
class WordSym a where 
    root :: Int -> a
    isRoot :: a -> Bool

instance WordSym GWord where  
    root _ = (mkWord "Root", mkPOS "ROOT")  
    isRoot (_, pos) = mkPOS "ROOT" == pos  

newtype Sentence word = Sentence (Array Int word)
    deriving (Show, Eq)

mkSentence :: [word] -> Sentence word
mkSentence words = Sentence $ listArray (1, length words) words

instance (Arbitrary word) => Arbitrary (Sentence word) where 
    arbitrary = do 
      NonEmpty ls <- arbitrary
      return $ mkSentence ls

instance (WordSym word, Show word) => SentenceLattice (Sentence word) where  
    type Symbol (Sentence word)= word
    sentenceLength = slength
    getWords (Sentence s) i = if i <=0 || i > n+1 then 
                                  throw $ AssertionFailed ("here" ++ show i ++ show s)
                              else  
                                  if i == n + 1 then [root i] else  [s ! i] 
        where n = slength (Sentence s)

slength :: Sentence b -> Int
slength (Sentence s) =  n
        where (1, n) = bounds s 

getWord :: (WordSym word, Show word) => Sentence word -> Int -> word 
getWord sent i = w
    where [w] =  getWords sent i

-- Sentence Lattice

newtype SentenceLat word = SentenceLat (Array Int [word])
    deriving Show

sllength (SentenceLat s) =  n
        where (1, n) = bounds s 

mkSentenceLat ::  (WordSym word) => [[word]] -> SentenceLat word
mkSentenceLat words = SentenceLat $ listArray (1, length words) $  words

instance (WordSym word) => SentenceLattice (SentenceLat word) where  
    type Symbol (SentenceLat word)= word
    sentenceLength = sllength
    getWords (SentenceLat s) i = if i == n + 1 then [root i] else  s ! i 
        where n = sllength (SentenceLat s)


