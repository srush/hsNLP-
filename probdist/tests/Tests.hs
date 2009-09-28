{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances #-}
module Main where 

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit

import NLP.Probability.Observation
import NLP.Probability.Distribution
import qualified Data.IntMap as IM

import qualified Data.Set as S
import Data.List
import Control.Monad (liftM)
import Debug.Trace
import qualified NLP.Probability.TrieWrap as TW
import Data.Monoid
main = defaultMain tests

type SampleEvent = Char


tests = 
        [testGroup "SemiRing Props"  [
                       testProperty "MLE" prop_mle]]

--instance Arbitrary Prob where 
--    arbitrary = Prob `liftM` choose (0.0, 1.0)

arbitraryPos = do 
  Positive a <- arbitrary
  return $ a 

instance (Enum event) => Arbitrary (Observed event) where 
    arbitrary = do  
        a <- listOf arbitraryPos
        b <- (arbitrary::Gen [Char])
        return $ Observed $ IM.fromList $ zip (map fromEnum b) a 


prop_mle a = all (\ (i,c)  -> (prob mle (toEnum i)) == (c / total )) $ IM.toList m 
    where types = (a::Observed Char)
          (Observed m) = a 
          mle = estimateMLE a 
          total = calcTotal a

normalize :: [Int] -> [Double]
normalize ls = map ((/ n) . fromIntegral)  ls  
    where n = fromIntegral $ sum ls 

prop_mixed ols cs = all (\c -> prob mix c == (sum $ zipWith (*) normProbs $ map (flip prob c ) mleDists) ) cs 
    where types = ((ols, cs)::([(Int, Observed Char)], [Char]))
          (nls, obs) = unzip ols
          normProbs = normalize nls
          mleDists =  map estimateMLE obs
          mix     = estimateMix (zip normProbs $ repeat estimateMLE) obs

prop_monoid trie = trie `mappend` mempty == trie && 
                   mempty `mappend` trie == trie
     where types = trie :: TW.SumTrie Char [Int]

prop_monoid2 trie1 trie2 (a,b,c) = 
     (TW.lookup key trie1 /= Nothing || 
      TW.lookup key trie2 /= Nothing)  ==>
     TW.lookup key (trie1 `mappend` trie2) ==  
    (TW.lookup key trie1 `mappend` TW.lookup key trie2)
     where types = (trie1,trie2, (a,b,c)) :: (TW.SumTrie Bool [Bool], TW.SumTrie Bool [Bool], (Bool, Bool, Bool))
           key = [a,b]