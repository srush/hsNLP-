{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
module NLP.Language.SimpleLanguage where

import Helpers.Common hiding (char, space)
import Helpers.Parse
import NLP.Atom

class Parsable a where 
    parser :: Parser a

newtype POS = POS String 
    deriving (Eq, Ord)
type APOS = Atom POS 
mkPOS = POS

instance Show POS where 
    show (POS p) = p
instance Read POS where 
    readsPrec _ =  makeParseRead parser
ignore a = do {a; return ()}
spaceSep p = manyTill p (ignore space <|> eof)
 
instance Parsable POS where 
    parser = POS `liftM` spaceSep anyChar

newtype NonTerm = NonTerm String
    deriving (Eq, Ord,Binary)

isWrapNPB = const False

type ANonTerm = Atom NonTerm
mkNonTerm = NonTerm

instance Show NonTerm where 
    show (NonTerm p) = p

instance Read NonTerm where 
    readsPrec _ =  makeParseRead parser

instance Parsable NonTerm where 
    parser = NonTerm `liftM` (many1 $ choice [upper, char '_'])

newtype Word = Word String
    deriving (Eq, Ord)
mkWord = Word
type AWord = Atom Word

instance Read Word where 
    readsPrec _ =  makeParseRead parser

instance Parsable Word where 
    parser = Word `liftM` spaceSep anyChar


instance Show Word where 
    show (Word p) = p



newtype GWord = GWord (AWord, APOS)
    deriving (Eq, Ord, Show, Binary)
getLex (GWord (l,_)) = l  
getPOS (GWord (_,p)) = p 

instance Pretty (Atom a) where 
    pPrint = text . show 
