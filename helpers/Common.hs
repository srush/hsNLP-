module Helpers.Common (
               csep,
               fromJustNote,
               fromJustDef,
               catMaybes,
               fromMaybe,
               readNote,
               intercalate, 
               liftM,
               atNote,
               ap,
               guard,
               on,
               separate,
               module Data.Binary,
               module Data.DeriveTH, 
               module Test.QuickCheck,
               module Text.PrettyPrint.HughesPJClass,
               module Data.Monoid,
               module Text.Printf,
               module Control.DeepSeq
               
              )
    where 
import Control.DeepSeq
import Text.PrettyPrint.HughesPJClass
import Data.Maybe (catMaybes, fromMaybe)
import Test.QuickCheck
import Data.Binary hiding (Word)
import Data.DeriveTH
import Safe
import Data.Function (on)
import Control.Monad (liftM, ap, guard)
import Data.List (intercalate)
import Data.Monoid
import Text.Printf
import Data.List (elemIndex)
csep = hsep .  punctuate comma . filter (not. isEmpty) 


separate :: (Eq el) => el -> [el] -> [[el]]
separate el [] = [] 
separate el ls = case elemIndex el ls of
                   Just n -> 
                       (take n ls): (separate el (drop (n+1) ls))
                   Nothing -> []
