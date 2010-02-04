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

instance Pretty NonTerm where 
    pPrint = text .show

instance Read NonTerm where 
    readsPrec _ =  makeParseRead parser

instance Parsable NonTerm where 
    parser = NonTerm `liftM` (many1 $ choice [upper, char '_'])

newtype Label = Label String 
type ALabel = Atom Label
mkLabel = Label

instance Read Label where 
    readsPrec _ =  makeParseRead parser

instance Parsable Label where 
    parser = Label `liftM` spaceSep anyChar

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

class WordSymbol a where 
    getPOS :: a -> APOS
    getLex :: a -> AWord

instance WordSymbol GWord where 
    getLex (GWord (l,_)) = l  
    getPOS (GWord (_,p)) = p 


instance Pretty (Atom a) where 
    pPrint = text . show 


newtype Triplet = Triplet (NonTerm, Maybe NonTerm, Maybe NonTerm)
    deriving (Eq,Ord)
 
instance Show Triplet where 
     show = render .pPrint

instance Pretty Triplet where 
    pPrint (Triplet (a,b,c)) = 
        hcat $ punctuate (text "+") $ map pPrint [Just a,b,c] 

instance Parsable Triplet where 
    parser = do 
      [a,b,c] <- choice [Just `liftM` (parser :: Parser NonTerm), 
                         char '*' >> (return Nothing)] 
                   `sepBy` char '+'
      return $ Triplet (fromJustNote "first" a,b,c) 

instance Read Triplet where 
    readsPrec _ =  makeParseRead parser

type ATriplet = Atom Triplet
type STriplet = (Atom NonTerm, Maybe ANonTerm, Maybe ANonTerm)