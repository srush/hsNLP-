{-# LANGUAGE GeneralizedNewtypeDeriving ,FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
module NLP.ParseMonad (ParseMonad, 
                       loadDebugMappers,
                       loadMappers,
                       runParseMonad,
                       isVerb,
                       isComma,
                       collapseWord,
                       testPM,
                       module NLP.Atom)
    where 
import Control.Monad.Trans
import NLP.Atom
import NLP.Language.SimpleLanguage
import NLP.Grammar.Spine
import Helpers.Common hiding (char)
import Control.Monad.Reader
import Helpers.Parse 
import qualified Data.Set as S

data Mappers = Mappers {
      map_pos   :: Mapper POS,
      map_word  :: Mapper Word,
      map_nt    :: Mapper NonTerm,
      map_spine :: Mapper (Spine NonTerm), 
      map_isComma :: S.Set APOS,
      map_isVerb  :: S.Set APOS,
      map_commonWords :: S.Set Word
    }

newtype ParseMonad a = ParseMonad (Reader Mappers a)
    deriving (Functor, Monad, MonadReader Mappers)

runParseMonad (ParseMonad pm) mappers = runReader pm mappers

instance Read (Spine NonTerm) where 
    readsPrec _ = makeParseRead $ parseSpine parser

data LoadMapConf = LoadMapConf {
      directory :: String,
      commas :: [String],
      verbs :: [String]
    }

defaultLoadMap = LoadMapConf "maps/" [ ","]  [ "VB", "VBD", "VBG","VBN", "VBP", "VBZ"] 

testPM pm = do 
  l <- loadDebugMappers
  return $ runParseMonad pm l

loadDebugMappers = loadMappers defaultLoadMap

loadSet file = do 
  contents <- readFile file
  return $ S.fromList $ map read $ lines contents

loadMappers :: LoadMapConf -> IO (Mappers)
loadMappers conf = do
  let dir = directory conf
  m1 <- loadMapper $ dir ++ "ftags.map"
  m2 <- loadMapper $ dir ++ "words.map"
  m3 <- loadMapper $ dir ++ "nt.map"
  m4 <- loadMapper $ dir ++ "spine.map"
  common <- loadSet $ dir ++ "commonWords.map"
  let commaSet = map (toAtomWithMap m1 . mkPOS) $ commas conf 
  let verbSet = map (toAtomWithMap  m1 . mkPOS) $ verbs conf
  return $ Mappers {
               map_pos = m1,
               map_word = m2,
               map_nt = m3,
               map_spine = m4,
               map_isComma = S.fromList commaSet,
               map_isVerb = S.fromList verbSet,
               map_commonWords = common
             }

collapseWord :: Word -> ParseMonad Word
collapseWord w = do 
  mapper <- ask 
  return $ if S.member w $ map_commonWords mapper then 
               w 
           else read "*UNK*" 
               
instance (MonadAtom POS ParseMonad) where 
    getMapper = map_pos `liftM` ask
instance (MonadAtom Word ParseMonad) where 
    getMapper = map_word`liftM` ask
instance (MonadAtom NonTerm ParseMonad) where 
    getMapper = map_nt `liftM` ask
instance (MonadAtom (Spine NonTerm) ParseMonad) where 
    getMapper = map_spine `liftM` ask

isComma :: ParseMonad (APOS -> Bool)
isComma = do
  m <- ask
  return $ \pos -> S.member pos (map_isComma m) 


isVerb :: ParseMonad (APOS -> Bool)
isVerb  = do
  m <- ask
  return $ \pos -> S.member pos (map_isVerb m) 
       