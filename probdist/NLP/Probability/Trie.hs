{-# LANGUAGE DeriveDataTypeable #-}
module NLP.Probability.Trie where 
import Data.Monoid
import qualified Data.Map as M
import Control.Monad (foldM, liftM)
import Data.Maybe (catMaybes)
import Safe (fromJustNote)
import Text.Printf
import Data.List (intercalate)
import Test.QuickCheck
import Data.Generics
import Data.Binary

data Trie letter holder = 
    Node holder (M.Map letter (Trie letter holder))
         deriving (Eq, Ord, Typeable, Data)

instance (Binary holder, Ord letter, Binary letter) => (Binary (Trie letter holder)) where 
    put (Node holder m)= put holder >> put m
    get = do
      holder <- get
      m <- get
      return $ Node holder m

instance (Ord letter, Arbitrary letter, Arbitrary holder) => Arbitrary (Trie letter holder) where 
    arbitrary = do
      holder <- arbitrary
      child <- frequency [(40,vectorOf 2 arbitrary), (60,return $ mempty)] 
      return $ Node holder (M.fromList child)


instance (Show letter, Show holder) => Show (Trie letter holder) where 
    show (Node holder m) = printf "%s->{%s}" (show holder) 
                           (intercalate ", " $ map show $ M.toList m)

instance (Ord letter) => Functor (Trie letter) where 
    fmap f (Node h m)  = Node (f h) (M.map (fmap f) m)

instance (Monoid holder, Ord letter) => Monoid (Trie letter holder) where 
    mempty = Node mempty mempty
    mappend (Node a m) (Node a' m') = Node (a `mappend` a') (M.unionWith mappend m m')


find :: (Ord letter) => [letter] -> Trie letter holder -> Maybe holder
find letters m = 
  holder `liftM` foldM (flip step) m letters 

holder (Node h _) = h 

step :: (Ord letter) => letter -> Trie letter holder -> Maybe (Trie letter holder) 
step l (Node _ m) = M.lookup l m

addColumn :: (Ord letter, Monoid holder) => 
             [letter] -> holder -> Trie letter holder -> Trie letter holder 
addColumn letters aholder n = fromJustNote "add column" $ loop letters aholder (Just n)
    where loop (l:letters) aholder n =
              case n of 
                (Just (Node h m)) ->   
                    Just $ Node (h `mappend` aholder) $ M.alter (next) l m
                Nothing -> 
                    Just $ Node aholder $ M.singleton l $ fromJustNote "next" $ next Nothing
              where  next = loop letters aholder
          loop [] aholder (Just (Node h m)) = 
              Just $ Node (h `mappend` aholder) m
          loop [] aholder Nothing =
              Just $ Node aholder mempty

columnWithDefault :: (Ord letter) => holder -> [letter] -> Trie letter holder -> [holder] 
columnWithDefault def (l:letters) (Node h m) = 
    h : (maybe (replicate (length letters) def) (columnWithDefault def letters) $ step l (Node h m))

prune :: (Ord letter) => (holder -> Bool) -> Trie letter holder -> Trie letter holder
prune filter (Node h m) = Node h (M.map (prune filter) newMap)
    where newMap = M.filter (\ (Node h m) -> filter h) m  

expand :: (Ord letter) => Trie letter holder -> [[(Maybe letter, holder)]]
expand (Node h m) = expand' m [(Nothing, h)]
    where
      expand' m soFar =
              if M.null m then [reverse $ soFar]
              else concat $ map expandOne $ M.toList m 
                  where expandOne (l, Node h m) = expand' m ((Just l,h):soFar)

expand_ :: (Ord letter) => Trie letter holder -> [([letter], [holder])]
expand_ = map (\(letters, holder) -> (catMaybes letters, holder)) . map unzip. expand


prop_monoid trie = trie `mappend` mempty == trie && 
                   mempty `mappend` trie == trie
    where types = trie :: Trie Char [Int]

prop_monoid2 trie1 trie2 (a,b,c) = 
    (find key trie1 /= Nothing || 
     find key trie2 /= Nothing)  ==>
    find key (trie1 `mappend` trie2) ==  
    (find key trie1 `mappend` find key trie2)
    where types = (trie1,trie2, (a,b,c)) :: (Trie Bool [Bool], Trie Bool [Bool], (Bool, Bool, Bool))
          key = [a,b]