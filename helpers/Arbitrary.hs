module Helpers.Arbitrary where 
import Test.QuickCheck
import Data.Array
import Data.List (partition, sort, nub, inits)
 
alpha = ['a'..'z'] ++ ['A'..'Z']
basicChar =  alpha ++ ['.', '`','\'', ',']

positive :: Gen Int
positive = do 
  Positive p <- arbitrary
  return p

nonEmptyArray nt = do
  NonEmpty ls <- arbitrary
  return $ nt $ listArray (1, length ls) ls 
      
partitionRange (i,j) = 
    if j < i then
        return []
    else do  
      splits <- listOf $ choose (i,j)
      let allSplit = [i] ++ (nub $ sort $ splits) ++ [j] 
      return $ zip allSplit $ tail allSplit
