{-# LANGUAGE  GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Word (mkWord, Word) where 
import Common
import qualified Data.Trie as T 
import qualified Data.IntMap as IM
import qualified Data.ByteString.Char8 as BS
import System.IO.Unsafe

wordFile = "/tmp/allwords"
{-# NOINLINE wordTable #-}
wordTable :: (T.Trie Int, IM.IntMap String)
wordTable = unsafePerformIO $ decodeFile wordFile

mkWord :: String -> Word
mkWord str = Word $ fromMaybe (fromJustNote "WordTable" $ T.lookup (BS.pack "*UNK*") $ fst wordTable ) $ T.lookup (BS.pack str) $ fst wordTable

newtype Word = Word Int 
    deriving (Eq, Ord, Enum, Binary)

instance Arbitrary Word where 
    arbitrary = Word `liftM` choose (fromEnum $ (minBound::Word), fromEnum $ (maxBound::Word))

instance Show Word where 
    show (Word i) = fromJustNote "WordTable" $ IM.lookup i $ snd wordTable

instance Pretty Word where 
    pPrint = text . show  

instance Bounded Word where 
    minBound = Word 0
    maxBound = Word 9996 -- hack! 
