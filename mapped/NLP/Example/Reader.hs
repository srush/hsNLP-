{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
module NLP.Example.Reader where 
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import NLP.Atom
import Control.Monad 
import Control.Monad.Reader
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

data AtomReader = AtomReader {
      mpos :: Mapper POS,
      mpos2 :: Mapper POS2
}

newtype ParseMonad a = ParseMonad (Reader AtomReader a)
    deriving (Functor, Monad, MonadReader AtomReader)

instance (MonadAtom POS ParseMonad) where 
    getMapper = mpos `liftM` ask

instance (MonadAtom POS2 ParseMonad) where 
    getMapper = mpos2 `liftM` ask

action :: ParseMonad String
action = (do
           atom <-  toAtom $ POS2 "NN"
           atom2 <- toAtom $ POS "NN" 
           return "hello")

runExample = do
  mapper <- loadMapper "ftags.mapped" :: IO (Mapper POS)
  mapper2 <- loadMapper "ftags.mapped" :: IO (Mapper POS2)
  let readata = AtomReader mapper mapper2
  let (ParseMonad a) = action
  let m = runReader a readata
  print $ m
    
