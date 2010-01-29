{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
module NLP.Language.SimpleLanguage where

import Helpers.Common
import NLP.Atom

newtype POS = POS String 
type APOS = Atom POS 

newtype NonTerm = NonTerm String
type ANonTerm = Atom NonTerm

newtype Word = Word String
type AWord = Atom Word

newtype GWord = GWord (AWord, APOS)

getLex (GWord (l,_)) = l  
getPOS (GWord (_,p)) = p 

