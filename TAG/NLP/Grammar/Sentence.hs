module NLP.Grammar.Sentence where 
import NLP.Grammar.Common


newtype Sentence word = Sentence (Array Int word)
    deriving (Show, Eq)

mkSentence :: [word] -> Sentence word
mkSentence words = Sentence $ listArray (1, length words) words

instance (Arbitrary word) => Arbitrary (Sentence word) where 
    arbitrary = do 
      NonEmpty ls <- arbitrary
      return $ mkSentence ls

sentenceLength :: Sentence b -> Int
sentenceLength (Sentence s) =  n
        where (1, n) = bounds s 

getWord :: (WordSym word, Show word) => Sentence word -> Int -> word 
getWord sent i = w
    where [w] =  getWords sent i
