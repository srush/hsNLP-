module LP.Format.Graph where 

--{{{  Imports 
import Helpers.Common
import Data.Array
import qualified Data.Map as M 
import qualified Data.Bimap as BM 
import System.Process
import System.IO
import Control.Monad
import qualified Data.ByteString.Char8 as S
import Data.ByteString (ByteString) 

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

getOn :: (Ord node) => ([Int], [(Int,Double)]) -> Graph node -> ([node], [(node,Double)]) 
getOn (parts,residual) graph = (on, res) 
    where n = nodeCount graph
          toNode i = fromJustNote "graph2" $  BM.lookup i $ nodeMap graph
          on = mconcat $ map (\(i,p) -> if p == 1 then [toNode i] 
                             else []) $ zip [1..n] parts
          res = do 
            (n, score) <- residual
            return (toNode (n+1), score) 

getOn2 :: (Ord node) => ([Int], [(Int,Double)]) -> Graph node -> ([node]) 
getOn2 (parts,residual) graph = on 
    where n = nodeCount graph
          toNode i = fromJustNote "graph2" $  BM.lookup i $ nodeMap graph
          on = mconcat $ map (\(i,p) -> if p == 1 then [toNode i] 
                             else []) $ zip [1..n] parts


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


writeReadable graph = 
    int (nodeCount graph) <+> int (edgeCount graph) <+> int 1 $$
    (vcat $ map (\(i, (i1,i2)) -> text (show $ toNode i) <+> double i1 <+> double i2) $ zip [1..] $ elems $ ss graph) $$ text ""
        where    
          toNode i = fromJustNote "graph" $  BM.lookup i $ nodeMap graph

writeGraph graph = 
    int (nodeCount graph) <+> int (edgeCount graph) <+> int 1 $$
    (vcat $ map (\(i1,i2) -> double i1 <+> double i2) $ elems $ ss graph) $$ text ""


writeEdges graph = 
    (vcat $ map (\ (i,el) -> if null el then empty else (vcat $  map (\(j,w) -> int i <+> int (j-1) <+> double w <+> double 0.0) el)) $ zip [0..] $ elems $ adj graph)


parseSolution nodeCount str = (ons, residual)
    where 
      l = lines str
      ons = map read $ take nodeCount l
      residual = map readRes $ drop nodeCount l 
      readRes s = (read a, read b)
          where (a, b) = break (== ' ') s


solveGraph :: (Ord node, Show node) =>   Int -> Graph node -> IO ([node])
solveGraph i graph = do 
  h <- openFile ("/tmp/tmp.graph"++show i) WriteMode 
  hPutStr h $ render $ writeGraph graph
  hFlush h
  hClose h

  when (i==0) $ do   
      h <- openFile "/tmp/tmp.edge" WriteMode 
      hPutStr h $ render $ writeEdges graph
      hFlush h
      hClose h

  ---let stdin = 
  system ("cat /tmp/tmp.graph"++show i ++ " /tmp/tmp.edge | ~/Lib/maxflow-v3.0.src/input > /tmp/tmp.graph.sol"++show i ) 
  contents <- S.readFile ("/tmp/tmp.graph.sol" ++ show i)

  h <- openFile ("/tmp/tmpRead.graph"++show i) WriteMode 
  hPutStr h $ render $ writeReadable graph
  hFlush h
  hClose h
         
  return $ getOn2 (parseSolution (nodeCount graph) $ S.unpack contents) graph


