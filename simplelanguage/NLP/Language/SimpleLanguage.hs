{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
module NLP.Language.SimpleLanguage where

import Helpers.Common hiding (char, space)
import Helpers.Parse
import NLP.Atom

class Parsable a where 
    parser :: Parser a

newtype POS = POS String 
    deriving (Eq, Ord, Show, Read)

type APOS = Atom POS 
mkPOS = POS

instance AtomRead POS where 
    atomRead =  makeParseAtomRead parser

ignore a = do {a; return ()}
spaceSep p = manyTill p (ignore space <|> eof)
 
instance Parsable POS where 
    parser = POS `liftM` (many1 $ choice [upper, oneOf "$#-`':,."])

newtype NonTerm = NonTerm String
    deriving (Eq, Ord, Binary, Show, Read)

--isWrapNPB = const False

type ANonTerm = Atom NonTerm
mkNonTerm = NonTerm

instance Pretty NonTerm where 
    pPrint (NonTerm n) = text n 

instance AtomRead NonTerm where 
    atomRead =  makeParseAtomRead parser

instance Parsable NonTerm where 
    parser = NonTerm `liftM` (many1 $ choice [upper, char '_'])

newtype Label = Label String 
    deriving (Show, Read)

type ALabel = Atom Label
mkLabel = Label

instance AtomRead Label where 
    atomRead =  makeParseAtomRead parser

instance Parsable Label where 
    parser = Label `liftM` spaceSep anyChar

newtype Word = Word String
    deriving (Eq, Ord, Show, Read)

instance Pretty Word where 
    pPrint (Word w) = text w


instance Pretty POS where 
    pPrint (POS p) = text p

mkWord = Word
type AWord = Atom Word

instance Parsable Word where 
    parser = Word `liftM` spaceSep anyChar

instance AtomRead Word where 
    atomRead =  makeParseAtomRead parser

newtype GWord = GWord (AWord, APOS)
    deriving (Eq, Ord, Show, Binary, Read)

class WordSymbol a where 
    getPOS :: a -> APOS
    getLex :: a -> AWord

instance WordSymbol GWord where 
    getLex (GWord (l,_)) = l  
    getPOS (GWord (_,p)) = p 


instance Pretty (Atom a) where 
    pPrint = text . show 


newtype Triplet = Triplet (NonTerm, Maybe NonTerm, Maybe NonTerm)
    deriving (Eq,Ord, Show, Read)
 
instance Pretty Triplet where 
    pPrint (Triplet (a,b,c)) = 
        hcat $ punctuate (text "+") $ map pPrint [Just a,b,c] 

instance Parsable Triplet where 
    parser = do 
      [a,b,c] <- choice [Just `liftM` (parser :: Parser NonTerm), 
                         char '*' >> (return Nothing)] 
                   `sepBy` char '+'
      return $ Triplet (fromJustNote "first" a,b,c) 

instance AtomRead Triplet where 
    atomRead =  makeParseAtomRead parser


type ATriplet = Atom Triplet
type STriplet = (Atom NonTerm, Maybe ANonTerm, Maybe ANonTerm)