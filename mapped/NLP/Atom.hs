{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, ScopedTypeVariables, FlexibleContexts, UndecidableInstances #-}
module NLP.Atom where
import qualified Data.Text as T
import qualified Data.Bimap as B
import System.IO
import Safe
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Cont
import Control.Monad.Error
import Control.Monad.Writer
import Control.Applicative 
import Data.Binary
import Control.DeepSeq
-- | NLP.Mapped - For large enumeration types that have a predefined 
-- bijection with the integers

newtype Mapper s = Mapper (B.Bimap s Int)  

newtype Atom s = Atom Int
    deriving (Show, Read, Eq, Ord, Binary, NFData)

newtype AtomM s a = AtomM (Reader (Mapper s) a)
    deriving (Monad, MonadReader (Mapper s))

class (Monad m) => MonadAtom s m where 
    getMapper :: m (Mapper s) 

toAtomWithMap :: (Show s, Ord s) => Mapper s -> s -> Atom s 
toAtomWithMap (Mapper m) a = 
    Atom $ fromJustNote ("Cannot map item " ++ show a)  $ B.lookup a m


fromAtomWithMap :: (Ord s, Show s) => Mapper s -> Atom s -> s 
fromAtomWithMap (Mapper m) (Atom b) = 
    fromJustNote ("Cannot int back to item " ++ show b) $ B.lookupR b m
     
toAtom :: (Show s, MonadAtom s m, Ord s) => s -> m (Atom s) 
toAtom a = do 
      m <- getMapper
      return $ toAtomWithMap m a 

fromAtom :: (Show s, MonadAtom s m, Ord s) => Atom s -> m s 
fromAtom b = do
  m <- getMapper
  return $ fromAtomWithMap m b

enumerate :: (MonadAtom s m) => m [s]
enumerate = do
  (Mapper m) <- getMapper
  return $ B.keys m

class AtomRead a where 
    atomRead :: String -> a

instance (Ord s) => MonadAtom s (AtomM s)  where 
    getMapper = ask

newtype AtomT s m a = AtomT (ReaderT (Mapper s) m a)
    deriving (Functor, Monad, MonadIO, MonadReader (Mapper s), MonadPlus, MonadTrans)

instance (Ord s, Monad m) => MonadAtom s (AtomT s m) where 
    getMapper = ask 

instance Enum (Atom a) where 
    fromEnum (Atom a) = a
    toEnum = Atom

readMapped :: (Ord a, AtomRead a) => String -> Mapper a
readMapped contents = 
    Mapper bm
        where all = map (T.split " ") $ T.lines $ T.pack contents
              bm = B.fromList $ map (\ [b, a] -> (atomRead $ T.unpack a, readNote "int atom read" $ T.unpack b)) all
                   
loadMapper :: (Ord a, AtomRead a) => String -> IO (Mapper a)
loadMapper file = do 
  readMapped `liftM` readFile file 

runAtom :: Mapper s -> AtomM s a -> a
runAtom mapper (AtomM m) = runReader m mapper  

runAtomT :: Mapper s -> AtomT s m a -> m a 
runAtomT mapper (AtomT m) = runReaderT m mapper  

showAtom :: (MonadAtom s m, Ord s, Show s) => Atom s -> m String
showAtom a = do 
  t <- fromAtom a 
  return $ show t

class  (Monad m) => UnAtom atom unatom m | atom -> unatom where 
    unAtom :: atom -> m unatom

instance (UnAtom b a m) => UnAtom (Maybe b) (Maybe a) m where 
    unAtom Nothing = return Nothing 
    unAtom (Just a) = Just `liftM` unAtom a 

instance (Show a, Ord a, MonadAtom a m) => UnAtom (Atom a) a m where 
    unAtom a = fromAtom a

 
