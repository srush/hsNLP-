{-# LANGUAGE ScopedTypeVariables #-}
module Main where 

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit


import qualified Data.Set as S
import Data.List
import Control.Monad (liftM)

main = defaultMain tests

tests = []
--        testGroup "SemiRing Props"  [
--                       testProperty "semiProb bool" prop_boolRing,
  
--instance Arbitrary Prob where 
--    arbitrary = Prob `liftM` choose (0.0, 1.0)

