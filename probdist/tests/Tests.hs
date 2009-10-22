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
import Data.Monoid
main = defaultMain tests

type SampleEvent = Char


tests = 
        [  []]

