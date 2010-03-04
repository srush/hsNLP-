{-# LANGUAGE OverloadedStrings #-}
module NLP.SMT.PhraseTable where 
import Helpers.Parse
import Helpers.Test
import qualified Data.Text as T 

newtype NonTerm = NonTerm String 
    deriving (Eq, Show)
newtype Word = Word String
    deriving (Eq, Show) 
type Mixed = Either NonTerm Word  

data PhraseRule = PhraseRule {
      parent :: NonTerm,
      english :: [Mixed],
      chinese :: [Mixed],
      prob :: [Double]
    } deriving (Eq, Show)


parseEither str = 
    if head str == '[' then Left $ parseNonTerm str 
    else Right $ Word str

parseNonTerm termstr = NonTerm $ tail $ init termstr  

parsePhraseRule str = PhraseRule {
                        parent = parseNonTerm $ T.unpack $ splits !! 0,
                        english = map (parseEither . T.unpack) $ T.split " " $ splits !! 1,
                        chinese = map (parseEither . T.unpack) $ T.split " " $ splits !! 2,
                        prob = map (read . T.unpack) $ T.split " " $ splits !! 3
                      }
    where 
      splits = map T.strip $ T.split "|||" $ T.pack str
    


-- TESTS
linePair = ("[C] ||| of 150,000 U.S. dollars , ||| 15 Chinese weird stuff ||| 2.8644771060e-08 0.0000000000e+00 0.0000000000e+00 4.1963022110e-06 1.1550410131e-02",
            PhraseRule {
              parent = NonTerm "C",
              english = map (Right .Word) ["of", "150,000", "U.S.", "dollars", ","],
              chinese = map (Right .Word) ["15", "Chinese", "weird", "stuff"],
              prob    = [2.8644771060e-08, 0.0, 0.0, 4.1963022110e-06, 1.1550410131e-02] 
            })

testPhrase = testGroup "Phrase Tests" [
               testCase "test read" test_read
             ]

test_read = assertEqual "parse test" (parsePhraseRule line) res 
    where (line, res) = linePair  
    
