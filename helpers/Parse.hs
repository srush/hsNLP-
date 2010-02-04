module Helpers.Parse (
                      module Text.ParserCombinators.Parsec,
                      makeParseRead,
                      nat,
                      float
                     ) where
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as P

makeParseRead parsecRead = either (const []) id . parse parsecRead' "" where
    parsecRead' = do a <- parsecRead
                     rest <- getInput
                     return [(a, rest)]
lexer = (P.makeTokenParser emptyDef) 

nat  = P.natural lexer
float  = P.float lexer
