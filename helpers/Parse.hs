module Helpers.Parse (
                      module Text.ParserCombinators.Parsec,
                      makeParseRead,
                     ) where
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char


makeParseRead parsecRead = either (const []) id . parse parsecRead' "" where
    parsecRead' = do a <- parsecRead
                     rest <- getInput
                     return [(a, rest)]
