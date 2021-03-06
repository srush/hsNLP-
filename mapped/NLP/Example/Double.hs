{-# LANGUAGE FlexibleContexts #-}
module NLP.Mapper.Double where 

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import NLP.Atom
import Control.Monad 
import Control.Monad.Trans
makeRead parsecRead = either (const []) id . parse parsecRead' "" where
    parsecRead' = do a <- parsecRead
                     rest <- getInput
                     return [(a, rest)]

newtype POS = POS String 
    deriving (Show, Eq, Ord)

instance Read POS where 
    readsPrec _ =  makeRead $ (POS `liftM` many anyChar)

newtype POS2 = POS2 String 
    deriving (Show, Eq, Ord)

instance Read POS2 where 
    readsPrec _ =  makeRead $ (POS2 `liftM` many anyChar)

type ParseMonad = AtomT POS (AtomT POS2 IO) 

grabAtom :: (MonadAtom POS2 m) => m (Atom POS2)
grabAtom = do 
  toAtom $ POS2 "NN"
  

runExample = do 
  mapper <- loadMapper "ftags.mapped" :: IO (Mapper POS)
  mapper2 <- loadMapper "ftags.mapped" :: IO (Mapper POS2)
  m <- runAtomT mapper2 $ runAtomT mapper $  (do
        atom <-  lift $ grabAtom
        atom2 <- toAtom $ POS "NN" 
        return "hello")
  print $ m