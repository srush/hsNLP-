{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module NLP.Atom where
import qualified Data.Text as T
import qualified Data.Bimap as B
import Helpers.Common
import System.IO
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Cont
import Control.Monad.Error
import Control.Monad.Writer
import Control.Applicative 

-- | NLP.Mapped - For large enumeration types that have a predefined 
-- bijection with the integers

newtype Mapper s = Mapper (B.Bimap s Int)  

newtype Atom s = Atom Int
    deriving (Show, Eq, Ord)

newtype AtomM s a = AtomM (Reader (Mapper s) a)
    deriving (Monad, MonadReader (Mapper s))

class (Monad m) => MonadAtom s m | m -> s where 
    fromAtom :: Atom s -> m s
    toAtom :: s -> m (Atom s)

toA a = do 
      (Mapper m) <- ask
      return $ Atom $ fromJustNote "Cannot map item" $ B.lookup a m

fromA (Atom b) = do
  (Mapper m) <- ask
  return $ fromJustNote "Cannot int back to item" $ B.lookupR b m

instance (Ord s) => MonadAtom s (AtomM s)  where 
    toAtom = toA
    fromAtom  = fromA

newtype AtomT s m a = AtomT (ReaderT (Mapper s) m a)
    deriving (Functor, Monad, MonadIO, MonadReader (Mapper s), MonadPlus, MonadTrans)

instance (Ord s, Monad m) => MonadAtom s (AtomT s m) where 
    toAtom = toA
    fromAtom  = fromA

instance Enum (Atom a) where 
    toEnum = Atom 
    fromEnum (Atom a) = a

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

showAtom :: (MonadAtom s m, Show s) => Atom s -> m String
showAtom a = do 
  t <- fromAtom a 
  return $ show t
