{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
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
    deriving (Show, Eq, Ord, Binary, NFData)

newtype AtomM s a = AtomM (Reader (Mapper s) a)
    deriving (Monad, MonadReader (Mapper s))

class (Monad m) => MonadAtom s m where 
    getMapper :: m (Mapper s) 

toAtomWithMap :: (Ord s) => Mapper s -> s -> Atom s 
toAtomWithMap (Mapper m) a = 
    Atom $ fromJustNote "Cannot map item" $ B.lookup a m
         
toAtom :: (MonadAtom s m, Ord s) => s -> m (Atom s) 
toAtom a = do 
      m <- getMapper
      return $ toAtomWithMap m a 

fromAtom :: (MonadAtom s m, Ord s) => Atom s -> m s 
fromAtom (Atom b) = do
  (Mapper m) <- getMapper
  return $ fromJustNote "Cannot int back to item" $ B.lookupR b m

instance (Ord s) => MonadAtom s (AtomM s)  where 
    getMapper = ask

newtype AtomT s m a = AtomT (ReaderT (Mapper s) m a)
    deriving (Functor, Monad, MonadIO, MonadReader (Mapper s), MonadPlus, MonadTrans)

instance (Ord s, Monad m) => MonadAtom s (AtomT s m) where 
    getMapper = ask 

readMapped :: (Ord a, Read a) => String -> Mapper a
readMapped contents = 
    Mapper bm
        where all = map (T.split " ") $ T.lines $ T.pack contents
              bm = B.fromList $ map (\ [b, a] -> (read $ T.unpack a, read $ T.unpack b)) all
                   
loadMapper :: (Ord a, Read a) => String -> IO (Mapper a)
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
