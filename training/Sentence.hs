{-# LANGUAGE TypeFamilies, TypeSynonymInstances, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Sentence where 
import NLP.ChartParse 
import NLP.Semiring
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
    root :: a


instance WordSym GWord where  
    root = (Word "Root", POS "ROOT")  


instance Arbitrary Word where 
    arbitrary = Word `liftM` (listOf1 $ elements basicChar)

instance Show Word where 
    show (Word w) = w

newtype Sentence semi word = Sentence (Array Int (semi,word))
    deriving (Show, Eq)


mkSentence :: (Semiring semi) => [(semi, word)] -> Sentence semi word
mkSentence words = Sentence $ listArray (1, length words) words

mkSimpleSentence :: (Semiring semi) => [word] -> Sentence semi word
mkSimpleSentence words = Sentence $ listArray (1, length words) $ map (\w -> (one, w)) $ words

instance (Arbitrary word, Semiring semi) => Arbitrary (Sentence semi word) where 
    arbitrary = do 
      NonEmpty ls <- arbitrary
      return $ mkSimpleSentence ls

instance (Semiring semi, WordSym word) => SentenceLattice (Sentence semi word) where  
    type Symbol (Sentence semi word)= word
    type LatticeSemi (Sentence semi word)  = semi
    sentenceLength = slength
    getWords (Sentence s) i = if i == n + 1 then [(one, root)] else  [s ! i] 
        where n = slength (Sentence s)


slength :: Sentence a b -> Int
slength (Sentence s) =  n
        where (1, n) = bounds s 


getWord sent i = w
    where [(_, w)] =  getWords sent i