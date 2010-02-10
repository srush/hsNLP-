{-# LANGUAGE GeneralizedNewtypeDeriving ,FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
module NLP.ParseMonad (ParseMonad, 
                       LoadMapConf (..),
                       Mappers,
                       defaultLoadMap,
                       loadDebugMappers,
                       loadMappers,
                       runParseMonad,
                       isVerb,
                       isComma,
                       isPunc,
                       isConj,
                       collapseWord,
                       testPM,
                       tripletMapper,
                       testParseMonad,
                       isNPB,
                       isRoot,
                       module NLP.Atom)
    where 

--{{{  Imports
import Control.Monad.Trans
import NLP.Atom
import NLP.Language.SimpleLanguage
import NLP.Grammar.Spine
import Helpers.Common hiding (char)
import Control.Monad.Reader
import Helpers.Parse 
import Helpers.Test
import qualified Data.Set as S
import qualified Data.Bimap as B
import qualified Data.Map as M
import Data.Char (ord)
--}}}

data Mappers = Mappers {
      map_pos   :: Mapper POS,
      map_word  :: Mapper Word,
      map_nt    :: Mapper NonTerm,
      map_spine :: Mapper (Spine NonTerm), 
      map_triplet :: Mapper Triplet, 
      map_nt2triplet :: M.Map (ANonTerm, Maybe ANonTerm, Maybe ANonTerm) ATriplet,
      map_isComma :: S.Set APOS,
      map_isVerb  :: S.Set APOS,
      map_isConj  :: S.Set APOS,
      map_isPunc  :: S.Set APOS,
      map_npb  :: ANonTerm,
      map_commonWords :: Maybe (S.Set Word)
    }

newtype ParseMonad a = ParseMonad (Reader Mappers a)
    deriving (Functor, Monad, MonadReader Mappers)

runParseMonad (ParseMonad pm) mappers = runReader pm mappers

instance Read (Spine NonTerm) where 
    readsPrec _ = makeParseRead $ parseSpine parser

data LoadMapConf = LoadMapConf {
      directory :: String,
      commas :: [String],
      verbs :: [String],
      conj :: [String],
      punc :: [String],
      npb :: String,
      shouldCollapseWords :: Bool
    }

defaultLoadMap = LoadMapConf "maps/" [",", ":"]  [ "VB", "VBD", "VBG","VBN", "VBP", "VBZ"] ["CC"] [".", "``", "''"]  "NPB" True

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
  m5 <- loadMapper $ dir ++ "triplets.map"
  common <- loadSet $ dir ++ "commonWords.map"
  let commaSet = map (toAtomWithMap m1 . mkPOS) $ commas conf 
  let verbSet = map (toAtomWithMap  m1 . mkPOS) $ verbs conf
  let conjSet = map (toAtomWithMap  m1 . mkPOS) $ conj conf
  let puncSet = map (toAtomWithMap  m1 . mkPOS) $ punc conf
  let (Mapper mtrip) = m5
  let nt2triplet = M.fromList [ ((toAtomWithMap m3 a, fmap (toAtomWithMap m3) b, fmap (toAtomWithMap m3) c),Atom i)  | 
        (Triplet (a,b,c),i) <- B.toList mtrip]
  return $ Mappers {
               map_pos = m1,
               map_word = m2,
               map_nt = m3,
               map_spine = m4,
               map_triplet = m5,
               map_nt2triplet = nt2triplet,
               map_isComma = S.fromList commaSet,
               map_isVerb = S.fromList verbSet,
               map_isConj = S.fromList conjSet,
               map_isPunc = S.fromList puncSet,
               map_npb    = toAtomWithMap m3 $ mkNonTerm $ npb conf ,
               map_commonWords = if (shouldCollapseWords conf) then Just common else Nothing
             }

tripletMapper :: ParseMonad (ANonTerm ->Bool, APOS -> Bool, STriplet -> Maybe ATriplet) 
tripletMapper = do 
  m <- ask
  cfn <- isComma
  let mtrip = map_nt2triplet m
      npb = map_npb m
     
  return ((==) npb ,cfn, \a -> 
            M.lookup a mtrip
         )

collapseWordMap :: Maybe (S.Set Word) -> Word -> Word 
collapseWordMap m w =
    case m of 
             Nothing -> w 
             Just common ->
                  if isNum w then
                      read "*NUM*"
                  else if S.member w common  then 
                      w 
                  else read "*UNK*" 
      where isNum (Word w ) = ord a >= ord '0' && ord a <= ord '9'                
                where a = w!!0

collapseWord :: Word -> ParseMonad Word
collapseWord w = do 
  mapper <- ask 
  let m = map_commonWords mapper
  return $ collapseWordMap m w

instance (MonadAtom POS ParseMonad) where 
    getMapper = map_pos `liftM` ask
instance (MonadAtom Word ParseMonad) where 
    getMapper = map_word`liftM` ask
instance (MonadAtom NonTerm ParseMonad) where 
    getMapper = map_nt `liftM` ask
instance (MonadAtom (Spine NonTerm) ParseMonad) where 
    getMapper = map_spine `liftM` ask

isGetter :: (Mappers-> S.Set APOS) ->  ParseMonad (APOS -> Bool)
isGetter get = do
  m <- ask
  return $ \pos -> S.member pos (get m)


isComma = isGetter map_isComma
isVerb = isGetter map_isVerb
isConj = isGetter map_isConj
isPunc = isGetter map_isPunc

isNPB :: ParseMonad (ANonTerm -> Bool)
isNPB = do 
  m <- ask
  return $ (== (map_npb m))


isRoot :: ParseMonad (ANonTerm -> Bool)
isRoot = do 
  a <- toAtom $ read "ROOT"
  return $ (== a)

--{{{  TESTS

testParseMonad = testGroup "ParseMonad props" [
                  testCase "collapse" test_collapseWord
                 ]


test_collapseWord = do 
  let tests = [("common", "hello", "hello"),
               ("uncommon", "hello2", "*UNK*"),
               ("number", "123", "*NUM*"),
               ("still number", "0.1", "*NUM*")]
  mapM_ (\(a,b,c)  -> assertEqual a (wm $ mkWord b) (mkWord c)) tests  
         where common = Just $ S.fromList $ map mkWord ["hello", "dog", "0.1"]
               wm = collapseWordMap common 

--}}}
