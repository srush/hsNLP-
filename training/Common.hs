module Common (
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
               module Debug.Trace,
               module Debug.Trace.Helpers,
               module Data.Monoid
              )
    where 
import Text.PrettyPrint.HughesPJClass
import Data.Maybe (catMaybes, fromMaybe)
import Test.QuickCheck
import Data.Binary hiding (Word)
import Data.DeriveTH
import Safe
import Data.Function (on)
import Control.Monad (liftM, ap, guard)
import Data.List (intercalate)
import Debug.Trace
import Debug.Trace.Helpers
import Data.Monoid