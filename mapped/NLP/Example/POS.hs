module NLP.Mapper.POS where 
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import NLP.Mapped
import Control.Monad 
newtype POS = POS String 
    deriving (Show, Eq, Ord)

makeRead parsecRead = either (const []) id . parse parsecRead' "" where
    parsecRead' = do a <- parsecRead
                     rest <- getInput
                     return [(a, rest)]

instance Read POS where 
    readsPrec _ =  makeRead $ (POS `liftM` many anyChar)


runExample = do 
  mapper <- loadMapper "ftags.mapped" 
  print $ runAtom mapper $ do
               atom <- toAtom $ POS "NN" 
               showAtom atom

               