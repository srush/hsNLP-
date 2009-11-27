module LP.Format.Graph where 

--{{{  Imports 
import Helpers.Common
import Data.Array
import qualified Data.Map as M 
import qualified Data.Bimap as BM 
import System.Process

--}}}

type Weight = Double

data Graph node = Graph { 
      adj :: Array Int [(Int, Weight)],
      ss  :: Array Int (Weight,Weight),
      nodeCount :: Int,
      edgeCount :: Int,
      nodeMap :: BM.Bimap Int node
    } 

instance (Show node, Ord node) =>  Show (Graph node) where 
    show graph = render $ vcat $ do
       (i, edges) <- zip [1..] $ elems $ adj graph
       (j, w) <- edges
       return $ text (show $ getName i) <+> text (show $ getName j) <+> text (show w) 

      where getName i = fromJustNote "node" $ BM.lookup i $ nodeMap graph  

partition :: (Ord node) => [Int] -> Graph node -> ([node],[node])
partition parts graph = 
    mconcat $ map (\(i,p) -> if p == 0 then ([toNode i],[]) 
                             else ([], [toNode i]) ) $ zip [1..n] parts
    where n = nodeCount graph
          toNode i = fromJustNote "graph" $  BM.lookup i $ nodeMap graph

mkGraph :: (Ord node) => [(node, (Weight, Weight))] -> [(node,node,Weight)] -> Graph node
mkGraph nodesWithWeights edges =
    Graph {
         adj = listArray (1,n) (map getNodes nodes),
         ss = listArray (1,n) sourceSink,
         nodeCount = n,
         edgeCount = length edges,
         nodeMap = nm 
    }
    where 
      (nodes, sourceSink) = unzip nodesWithWeights
      getNodes n = catMaybes $ map (\(a,b,w) ->  if a == n then Just (fromJustNote "graph" $ BM.lookupR b nm, w) 
                                                 else Nothing) edges
      n = length nodes 
      nm =  BM.fromList $ zip [1..] nodes   


writeGraph graph = 
    int (nodeCount graph) <+> int (edgeCount graph) <+> int 1 $$
    (vcat $ map (\(i1,i2) -> double i1 <+> double i2) $ elems $ ss graph) $$
    (vcat $ map (\ (i,el) -> if null el then empty else (vcat $  map (\(j,w) -> int i <+> int (j-1) <+> double w <+> double 0.0) el)) $ zip [0..] $ elems $ adj graph)

solveGraph graph = do 
  writeFile "/tmp/tmp.graph" $ render $ writeGraph graph
  runCommand "~/Lib/maxflow-v3.0.src/input < /tmp/tmp.graph > /tmp/tmp.graph.part.2"
  contents <- readFile "/tmp/tmp.graph.part.2"
  return $ partition (map read $ lines contents) graph