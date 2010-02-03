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

csep = hsep .  punctuate comma . filter (not. isEmpty) 

