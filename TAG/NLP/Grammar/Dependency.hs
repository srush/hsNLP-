{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}
module NLP.Grammar.Dependency (Dependency(..), getHead, singletonDep, rootInd, DEdge(..), toList, splitMap, convertToTree, flattenDep, hasDependency, hasDependencyAndLabel,testDep) where 

--{{{  Imports
import Helpers.Common
import qualified Data.Map as M
import Data.List
import qualified Data.Tree as T
import Helpers.Test
--}}}

--{{{  AdjunctionSide
-- | AdjunctionSide tells us from which direction we should adjoin a new tree
data AdjunctionSide = ALeft | ARight
                    deriving (Eq, Ord, Enum, Bounded)

--{{{AdjunctionSide Classes

$( derive makeBinary ''AdjunctionSide)
$( derive makeNFData ''AdjunctionSide)
$( derive makeArbitrary ''AdjunctionSide)

instance Show AdjunctionSide where 
    show ALeft = "Left"
    show ARight = "Right"

instance Pretty AdjunctionSide where pPrint = text . show

--}}}
--}}}

-- | Dependency structure is modeled as a map from 
--   Children to parents 
newtype Dependency label = 
    Dependency (M.Map Int (DEdge label))
    deriving (Eq, Monoid) 

--{{{ Dependency Classes 
instance (Show label) => Show (Dependency label) where 
    show (Dependency dep) = "Dep: " ++ (show $ M.toList dep) 

instance Functor Dependency where 
    fmap f (Dependency d1) = Dependency $ M.map (\(DEdge h info ) -> DEdge h (f info) ) d1 
--}}}

-- | Get the parent of an index
getHead :: Dependency a -> Int -> Maybe (DEdge a)
getHead (Dependency m) i = M.lookup i m

-- | Get the top index in the tree
rootInd :: Dependency a -> Int
rootInd dep = n
    where (n,_) = M.findMax r
          r = reverseMap dep

singletonDep :: Int -> Int -> a -> Dependency a
singletonDep head child info = Dependency $ M.singleton child (DEdge head info)


-- | DependencyEdge - the node that it points to and the label on the endge
data DEdge label = DEdge {to :: Int,
                          info :: label}  
    deriving (Eq)

--{{{ DEdge Classes 
instance (Show label) => Show (DEdge label) where 
    show dedge = (show $ to dedge) ++ " lab: " ++ (show $ info dedge)   

instance (Eq a) => Ord (DEdge a) where 
    compare = compare `on` to
--}}}

type SplitMap info = M.Map Int ([DEdge info], [DEdge info])


toList :: Dependency info -> [(Int, Int)]
toList (Dependency m) = do
  (i, dedge) <- M.toList m
  return (i, to dedge) -- Child, parent

-- | Takes a map from children to head and reverses it 
-- so it faces from head to children  
reverseMap :: Dependency info -> M.Map Int [DEdge info]
reverseMap (Dependency m) = 
    M.foldWithKey (\k (DEdge node info) -> M.insertWith (++) node [DEdge k info]) M.empty m 


-- | Takes a map to integers a splits it into two ordered lists  
splitMap :: (Eq a, Ord a) => M.Map Int [DEdge a] -> M.Map Int ([DEdge a], [DEdge a])
splitMap m = 
    M.mapWithKey partAndSort m 
     where partAndSort key intls = (reverse $ sort bottom, sort top)
               where (bottom, top) = partition ((< key).to) intls

convertToTree m  = convert' (fst $ M.findMax m') Nothing 
    where convert' ind v =
              T.Node v  $ maybe [] (map (\d -> convert' (to d) (Just $ info d))) $ M.lookup ind m' 
          m' = reverseMap m

-- | Flattens the dependency representation into a list of nodes and their children
flattenDep :: (Eq a, Ord a) => Dependency a -> [(Int, ([DEdge a], [DEdge a] ))]
flattenDep dep  = 
    map (\i -> (i, M.findWithDefault ([],[]) i sMap)) [1..n]
    where 
          sMap = splitMap $ reverseMap dep
          n = rootInd dep

hasDependency :: Dependency a -> Int -> Int -> Bool
hasDependency (Dependency dm) head child =
    (Just head) == (fmap to $ M.lookup child dm)

hasDependencyAndLabel :: (Eq a) => Dependency a -> Int -> Int -> a -> Bool
hasDependencyAndLabel (Dependency dm) head child label =
    hasDependency (Dependency dm) head child && 
    (Just label) == (fmap info childLook)
        where childLook = M.lookup child dm

--{{{ TESTS

runTests = defaultMain [testDep]

data BinTree a = Leaf | BinTree Int a (BinTree a) (BinTree a)
               deriving Show
bintreeToDep (BinTree head _ l r) = 
    mappend (btd head l) (btd head r)
        where btd h bintree = 
                  case bintree of 
                    BinTree c i l r -> mconcat [singletonDep h c i, btd c l, btd c r]
                    Leaf -> mempty
bintreeToDep _ = mempty 
-- | make a binary tree 
arbTree l u =
    if l+1 == u then 
        return Leaf
    else if l + 1 == u -1 then do
        i <- arbitrary
        return $ BinTree (l+1) i Leaf Leaf
    else do
      me <- choose (l+1, u-1)
      left <- arbTree l me 
      right <- arbTree me u
      i <- arbitrary
      return $ BinTree me i left right

instance (Arbitrary a) => Arbitrary (Dependency a) where 
    arbitrary = do  
        n <- choose (1,20)
        tree <- arbTree 0 n
        return $ bintreeToDep tree 

testDep = testGroup "Dependency props" [
         testProperty "splitMap" prop_splitMap,
         testProperty "reverseMap" prop_reverseMap
        ]

prop_splitMap a = 
    all (\(m, (left, right)) -> all ((< m).to) left && all ((> m).to) right) $ M.toList splitm
    where
      types = a :: Dependency ()
      splitm = splitMap m
      m = reverseMap a 

prop_reverseMap a =  and $ map (\ (node, DEdge head _) -> elem node $ map to $ (M.!) revm head) $ M.toList m
     where revm = reverseMap a
           (Dependency m) = a 
           types = a::Dependency ()
--}}}