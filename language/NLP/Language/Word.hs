{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables #-} 

module NLP.Language.Word where 

import NLP.Language.Common
import qualified Data.Trie as T 
import qualified Data.IntMap as IM
import qualified Data.ByteString.Char8 as BS
import System.IO.Unsafe

newtype WordTable a = WordTable (T.Trie Int, IM.IntMap String)
    deriving (Binary)

class Word a where
    lookupWord :: WordWrap a -> String 
    lookupWord (WordWrap i) = 
        fromJustNote "WordWrapTable" $ IM.lookup i $ snd wt
            where (WordTable wt) = getWordTable :: (WordTable a)
    getWordTable :: WordTable a 
    mkWord :: String -> WordWrap a
    mkWord str = 
        WordWrap $ fromMaybe (fromJustNote "WordTable" $ 
                                       T.lookup (BS.pack "*UNK*") $ fst wt ) $ 
        T.lookup (BS.pack str) $ fst wt
     where (WordTable wt) = getWordTable :: (WordTable a) 

newtype WordWrap a = WordWrap Int 
    deriving (Eq, Ord, Enum, Binary)

{-# NOINLINE wordTable #-}
wordTable :: (Word a) => String -> WordTable a
wordTable = unsafePerformIO . decodeFile 

instance Arbitrary (WordWrap w) where 
    arbitrary = WordWrap `liftM` choose (fromEnum $ (minBound::WordWrap a), fromEnum $ (maxBound::WordWrap a))

instance (Word w) => Show (WordWrap w) where 
    show = lookupWord

instance (Word w) => Pretty (WordWrap w) where 
    pPrint = text . show  

instance Bounded (WordWrap w) where 
    minBound = WordWrap 1
    maxBound = WordWrap 9996 -- hack! 
