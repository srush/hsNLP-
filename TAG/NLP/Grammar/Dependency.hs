{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module NLP.Grammar.Dependency where 
import Helpers.Common
import qualified Data.Map as M
import Data.List
import qualified Data.Tree as T

-- | Dependency structure is modeled as a map from 
--   Children to parents 
newtype Dependency label = 
    Dependency (M.Map Int (DEdge label))
    deriving (Eq, Monoid) -- not sure why this needs to be ord

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
  return (i, to dedge)

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

convertToTree m showInd = convert' $ fst $ M.findMax m' 
    where convert' ind =
              T.Node word $ maybe [] (map (convert' .to)) $ M.lookup ind m' 
                  where word = show ind 
          m' = reverseMap m

-- | Flattens the dependency representation into a list of nodes and their children
flattenDep :: (Eq a, Ord a) => Dependency a -> [(Int, ([DEdge a], [DEdge a] ))]
flattenDep dep  = 
    map (\i -> (i, M.findWithDefault ([],[]) i sMap)) [1..n]
    where 
          sMap = splitMap $ reverseMap dep
          n = rootInd dep


--{{{ TESTS

prop_splitMap a = 
    all (\(m, (left, right)) -> all ((< m).to) left && all ((> m).to) right) $ M.toList splitm
    where
      types = a :: Dependency ()
      splitm = splitMap m
      m = reverseMap a 

prop_reverseMap a =  and $ map (\ (node, DEdge head _) -> elem node $ map to $ (M.!) revm head) $ M.toList m
    where revm = reverseMap a
          (Dependency m) = a 
--}}}