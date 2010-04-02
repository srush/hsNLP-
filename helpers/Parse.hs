module Helpers.Parse (
                      module Text.ParserCombinators.Parsec,
                      makeParseRead,
                      makeParseAtomRead,
                      nat,
                      float,
                      onlyfloat
                     ) where
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as P

makeParseRead parsecRead = either (const []) id . parse parsecRead' "" where
    parsecRead' = do a <- parsecRead
                     rest <- getInput
                     return [(a, rest)]

makeParseAtomRead parsecRead = either (error "parse failed") id . parse parsecRead "" where
                     


lexer = (P.makeTokenParser emptyDef) 

nat  = 
  P.natural lexer

float  = do 
  p <- optionMaybe (char '-')
  f <- P.naturalOrFloat lexer
  let f2 = case f of 
             Right a -> a
             Left b -> fromIntegral b
  return $ maybe f2 (const $ -f2) p 

onlyfloat  = do 
  P.float lexer