{-# LANGUAGE TypeFamilies #-}
module NLP.WordLattice where 
import Helpers.Common
import Data.Array

-- | A word lattice has multiple words per position
class WordLattice a where
    type Symbol a 
    getWords :: a -> Int -> [Symbol a]   
    latticeLength :: a -> Int

-- | Sentences are the simple case of the word lattice
newtype Sentence word = Sentence (Array Int word)
    deriving (Show, Eq)

mkSentence :: [word] -> Sentence word
mkSentence words = Sentence $ listArray (1, length words) words 


sentFromArray :: Array Int word -> Sentence word
sentFromArray = Sentence 

instance WordLattice (Sentence word) where  
    type Symbol (Sentence word)= word
    latticeLength (Sentence s) = n
        where (1, n) = bounds s
    getWords (Sentence s) i = [s ! i] 
        
getWord :: Sentence word -> Int -> word 
getWord sent i = head $ getWords sent i


-- Sentence Lattice

-- | More interesting sentence lattice
newtype SentenceLat word = SentenceLat (Array Int [word])
    deriving Show

mkSentenceLat :: [[word]] -> SentenceLat word
mkSentenceLat words = SentenceLat $ listArray (1, length words) $  words

instance WordLattice (SentenceLat word) where  
    type Symbol (SentenceLat word)= word
    latticeLength (SentenceLat s)= n
        where (1, n) = bounds s 
    getWords (SentenceLat s) i = s ! i 

-- TEST

instance (Arbitrary word) => Arbitrary (Sentence word) where 
    arbitrary = do 
      NonEmpty ls <- arbitrary
      return $ mkSentence ls
