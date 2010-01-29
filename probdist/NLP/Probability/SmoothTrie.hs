{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}
module NLP.Probability.SmoothTrie where 
import Data.Monoid
import qualified Data.ListTrie.Map as T
import qualified Data.ListTrie.Base.Map as LT
import Control.Monad (foldM, liftM)
import Data.Maybe (catMaybes, fromMaybe)
import Data.List (intercalate, inits)
import Test.QuickCheck
import Data.Binary
import Text.PrettyPrint.HughesPJClass
import qualified Data.ListTrie.Base.Map as M
import Control.DeepSeq

newtype SmoothTrie map letter holder= SmoothTrie (T.TrieMap map letter holder)
    deriving (Show, Binary, Functor)

instance (NFData letter, NFData holder, M.Map map letter) => NFData (SmoothTrie map letter holder) where 
    rnf (SmoothTrie st) = rnf $ T.toList st 

instance (M.Map map letter, Arbitrary letter, Arbitrary holder) => Arbitrary (SmoothTrie map letter holder) where 
    arbitrary = do
      holder <- arbitrary
      return $ SmoothTrie $ T.fromList holder 

instance (M.Map map letter, Pretty holder, Pretty letter) => Pretty (SmoothTrie map letter holder) where  
    pPrint (SmoothTrie t) = printRows 1 
         where 
           tlist = T.toList t
           printRows n = if null oflen then empty 
                         else 
                             (hang (text "Row " <> int n) 4  
                                  $ vcat $ map (\(k,v) -> (pPrint k) <+> (pPrint v)) oflen) $$ printRows (n + 1) 
               where oflen = filter ((== n).length.fst) tlist  
           
instance (Monoid holder, M.Map map letter) => Monoid (SmoothTrie map letter holder) where 
    mempty = SmoothTrie mempty
    mappend (SmoothTrie m) (SmoothTrie m') = SmoothTrie (T.unionWith mappend m m')
    mconcat sumtries = SmoothTrie $ T.unionsWith mappend $ [s | SmoothTrie s <-sumtries]

lookup ks (SmoothTrie t) = T.lookup ks t 

{-# INLINE lookupWithDefault #-}
lookupWithDefault def ks (SmoothTrie t) = fromMaybe def $  T.lookup ks t 

insert key val (SmoothTrie t) = SmoothTrie (T.insert key val t)

count (SmoothTrie t) = T.size t

holder st = T.lookup [] st   

addColumn :: (M.Map map letter, Monoid holder) => 
             [letter] -> holder -> SmoothTrie map letter holder -> SmoothTrie map letter holder 
addColumn letters holder trie = trie `mappend` (SmoothTrie trieColumn)  
   where trieColumn = mconcat $ zipWith T.singleton (inits letters) $ repeat holder