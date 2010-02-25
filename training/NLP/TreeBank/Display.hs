{-# LANGUAGE ScopedTypeVariables #-}
module NLP.TreeBank.Display where 

-- GraphViz
import Data.GraphViz
import Data.GraphViz.Commands
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree
import Control.Monad.State
import qualified Data.Traversable as T
import NLP.TreeBank.TreeBank
import Helpers.Common
import Data.Array
import NLP.Language.SimpleLanguage hiding (Label)
import NLP.ParseMonad
import NLP.Grammar.Spine
import Debug.Trace
showGraph gr = dot'  --trace (show dot) dot
    where 
      dot' = dot{graphStatements = (graphStatements dot){subGraphs =states } }
      states = map (\a -> if subGraphID a == (Just $ Int 100) then a{isCluster = False} else a) $ subGraphs $ graphStatements $ dot

      dot =  clusterGraphToDot True gr [GraphAttrs $ [Size $ PointD 14.0 8.0]]
                   (\l@(n,_) -> if (n `mod` 10) == 0 then C 100 $ N l
                                
                                else C (n `div` 10) $ N l)
                   (\(a::Int) ->  Just $ Int a)
                   (\a -> 
                        if a == 100 then 
                            [GraphAttrs $ [RankDir FromLeft, Rank SameRank]]
                        else
                            [GraphAttrs $ [RankDir FromTop, Rank SameRank]]
                   )
                   (\(_,word) -> [Label $ StrLabel $ word]) 
                   (\(_,_,ed) -> case ed of 
                                 Adj -> [Data.GraphViz.Style $ [SItem Dashed []]]
                                 Sp -> [ArrowHead noArrow]
                                 W -> [Data.GraphViz.Style $ [SItem Invisible []]]
                   )                                            

data EdgeT = Adj | W | Sp

wisToGraph :: WordInfoSent -> Gr (String) EdgeT  
wisToGraph (WordInfoSent wis) = 
    mkGraph nodes edges 
        where 
          (nodes, edges) = ([rootNode] ++wordNodes ++ spineNodes,
                            wordEdges ++ spineEdges ++ adjEdges
                           ) 
          allwords = elems wis
          wordStrs = map (show . wordStr) allwords
          wordNodes = zip [10,20..] wordStrs
          rootNode = (2, "ROOT")
          adjEdges = map (\wi -> (((adjoinInd wi) * 10) + (adjPos wi) + 2, (ind wi) * 10 + 1 + (length $ toList $ spine wi), Adj)) allwords
          (spineNodes,spineEdges) = 
              unzip $ concat $ zipWith  
                        (\i wi -> 
                             zipWith (\j nt -> ((i+j,nt),(i+j, i+j-1,Sp))) [1..] $ ((show $ posStr wi) : (map show $ toList $ spine wi))) [10,20..] allwords
          wordEdges = zipWith (\(i,_) (j,_) ->  (i,j,W)) wordNodes (tail wordNodes) 

          

main = do 
  mappers <- loadDebugMappers 
  simple <- readSentence "CodeTest/Simple.sent"
  let dot = showGraph $ wisToGraph $ runParseMonad simple mappers
  runGraphvizCanvas Dot  dot Xlib
  runGraphviz dot DotOutput "/tmp/tmp.dot" 