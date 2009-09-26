{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module DependencyStructure where 

import qualified Data.Tree as T
import qualified NLP.ChartParse as CP
import ArbitraryHelpers
import qualified Data.Map as M
import Data.Function (on) 
import Data.List (partition, sort, nub, inits) 
import Data.Maybe (catMaybes)
import Data.Graph.Inductive
import Test.QuickCheck
import Data.Monoid
import Data.Array
import Data.GraphViz
import Data.GraphViz.Types
import System.Process
import System.Random
import NLP.FSM.Simple
import NLP.Semiring
import Test.QuickCheck
import Safe (fromJustNote)
-- Root is index 0

newtype Dependency adjinfo = 
    Dependency (M.Map Int (DEdge adjinfo))
    deriving (Eq, Monoid, Ord) -- not sure why this needs to be ord

instance (Show label) => Show (Dependency label) where 
    show (Dependency dep) = "Dep: " ++ (show $ M.toList dep) 

data DEdge label = DEdge {to :: Int,
                           info :: label}  
    deriving (Eq)

instance (Show label) => Show (DEdge label) where 
    show dedge = (show $ to dedge) ++ " lab: " ++ (show $ info dedge)   

instance (Eq a) => Ord (DEdge a) where 
    compare = compare `on` to

instance (Arbitrary a ) => Arbitrary (Dependency a) where     
    arbitrary = do 
      n <- choose (1,50)
      arbDepMap n (const arbitrary)


singletonDep head child info = Dependency $ M.singleton child (DEdge head info)

getHead (Dependency m) i =  fromJustNote "no head" $ M.lookup i m 

arbDepMap :: Int -> (Int -> Gen info) -> Gen (Dependency info)      
arbDepMap n arbInfo = do       
    start <- choose (1,n)
    info <- arbInfo (n+1)
    heads <- findHeads (1, start, n)
    return $ Dependency $ M.fromList $ (start, DEdge (n+1) info) : heads 
        where  
          chooseCouple (i,j) = do
                ranges <- partitionRange (i,j)
                mapM chooseHead ranges
              where chooseHead (i, j) = do 
                      head <- choose (i,j)
                      return (i,head,j)
--          findHeads :: (Arbitrary a) => (Int, Int, Int) -> Gen [(Int, DEdge info)] 
          findHeads  (l, cur, r) = do
                left <- chooseCouple (l, cur-1)
                leftInfo <- arbInfo cur
                leftHeads <- mapM findHeads left
                
                right <- chooseCouple (cur+1, r)
                rightInfo <- arbInfo cur
                rightHeads <- mapM findHeads right

                return $ map (\(_,lcur,_) -> (lcur, DEdge cur leftInfo)) left ++ 
                       map (\(_,rcur,_) -> (rcur, DEdge cur rightInfo)) right ++
                       concat leftHeads ++
                       concat rightHeads 
                            
 
type SplitMap info = M.Map Int ([DEdge info], [DEdge info])

-- Takes a map from children to head and reverses it 
-- so it faces from head to children  
reverseMap :: Dependency info -> M.Map Int [DEdge info]
reverseMap (Dependency m) = 
    M.foldWithKey (\k (DEdge node info) -> M.insertWith (++) node [DEdge k info]) M.empty m 

prop_reverseMap a =  and $ map (\ (node, DEdge head _) -> elem node $ map to $ (M.!) revm head) $ M.toList m
    where revm = reverseMap a
          (Dependency m) = a 

-- Takes a map to integers a splits it into two ordered lists  
splitMap :: (Eq a) => M.Map Int [DEdge a] -> M.Map Int ([DEdge a], [DEdge a])
splitMap m = 
    M.mapWithKey partAndSort m 
     where partAndSort key intls = (reverse $ sort bottom, sort top)
               where (bottom, top) = partition ((< key).to) intls

prop_splitMap a = 
    all (\(m, (left, right)) -> all ((< m).to) left && all ((> m).to) right) $ M.toList splitm
    where
      types = a :: Dependency ()
      splitm = splitMap m
      m = reverseMap a 

convertToTree m showInd = convert' $ fst $ M.findMax m' 
    where convert' ind =
              T.Node word $ maybe [] (map (convert' .to)) $ M.lookup ind m' 
                  where word = show ind 
          m' = reverseMap m

convertToGraph (Dependency m) showInd  = 
          setID (Str "depend") $ 
          graphToDot True (gr::Gr Int ()) [] 
             (\(_, i) -> [Label (StrLabel $ showInd i)]) ig 
    where
      ig _ = [] 
      (gr, _) = mkMapGraph (0:(M.keys m))  -- nodes
                    (map (\(f, t) -> (f, to t, ())) 
             $ M.toList m) --edges 

showDotGraph gr = do 
  file <- randomIO
  let filename = "/tmp/depend." ++ show (file::Double) 
  writeFile filename s
  runCommand $ "dotty " ++ filename 
    where s = printDotGraph $ gr

flattenDep dep  = 
    map (\i -> (i, M.findWithDefault ([],[]) i sMap)) [1..n]
    where 
          sMap = splitMap $ reverseMap dep
          n = rootPos dep

rootPos dep = n
    where (n,_) = M.findMax r
          r = reverseMap dep
makeDependencyFSM :: 
    (Semiring semi, Ord wordinfo) => 
    (Int -> wordinfo) -> 
    (wordinfo -> wordinfo -> (Int, DEdge edgeinfo) -> semi) -> 
    Int -> 
    ([DEdge edgeinfo], [DEdge edgeinfo]) -> 
    (GraphWFSM Int wordinfo semi, GraphWFSM Int wordinfo semi) 
makeDependencyFSM getWordInfo mkSemi headi (left, right)  = 
    (makeDirFSM left, makeDirFSM right)
    where  makeDirFSM expObs= 
                      expectFSM $
                                    [(getWordInfo $ to edge,
                                      mkSemi word (getWordInfo $ to edge) $ (headi, edge))
                                     | edge <- expObs]
           word = getWordInfo headi




expectFSM transitions = 
    makeWFSM 0 n Nothing ((n, []):
           [ (i-1, [(trans, (i, semi))])
            | ((trans,semi), i) <- zip transitions [1..]])  
        where n = length transitions

instance Functor Dependency where 
    fmap f (Dependency d1) = Dependency $ M.map (\(DEdge h info ) -> DEdge h (f info) ) d1 

score (Dependency d1) (Dependency d2) = 
    catMaybes $ map (\(i, edge) -> if edge == (M.!) d2 i then Nothing
                                   else Just ((i,edge), (i,(M.!) d2 i))
                    ) $ M.toList d1 