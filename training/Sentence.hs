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

newtype POS = POS String 
    deriving ( Eq, Ord)

$( derive makeBinary ''POS )


instance Show POS where 
    show (POS s) = s

instance Arbitrary POS where 
    arbitrary = 
      POS `liftM` map toUpper `liftM` (listOf1 $ elements basicChar) 

newtype Word = Word String 
          deriving (Eq, Ord)

$( derive makeBinary ''Word )

type GWord = (Word, POS)

-- ROOT is a special symbol that we put at the end of the sentence 
class WordSym a where 
    root :: Int ->  a


instance WordSym GWord where  
    root i = (Word "Root", POS "ROOT")  


instance Arbitrary Word where 
    arbitrary = Word `liftM` (listOf1 $ elements basicChar)

instance Show Word where 
    show (Word w) = w

newtype Sentence word = Sentence (Array Int word)
    deriving (Show, Eq)

mkSentence :: [word] -> Sentence word
mkSentence words = Sentence $ listArray (1, length words) words

instance (Arbitrary word) => Arbitrary (Sentence word) where 
    arbitrary = do 
      NonEmpty ls <- arbitrary
      return $ mkSentence ls

instance (WordSym word) => SentenceLattice (Sentence word) where  
    type Symbol (Sentence word)= word
    sentenceLength = slength
    getWords (Sentence s) i = if i == n + 1 then [root i] else  [s ! i] 
        where n = slength (Sentence s)


slength :: Sentence b -> Int
slength (Sentence s) =  n
        where (1, n) = bounds s 

getWord :: (WordSym word) => Sentence word -> Int -> word 
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


