module NLP.Grammar.DependencySent (DependencySentence(..)) where 


--{{{  Imports
import NLP.Grammar.Dependency
import Data.Array
--}}}

data DependencySentence label word =
    DependencySentence {
      dsSent :: Array Int word,
      dsDep :: Dependency label
    }