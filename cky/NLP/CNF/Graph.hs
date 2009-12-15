{-# LANGUAGE  TemplateHaskell, GeneralizedNewtypeDeriving, TypeFamilies #-}
module NLP.CNF.Graph where 

-- Code to turn a set of CNF edges into a graph representation for graph cut. snip snip

--{{{  Imports

import Helpers.Common
import NLP.CNF.LP
import qualified Data.Set as S  
import qualified Data.Map as M
import NLP.CNF
import LP.Format.Graph
import Debug.Trace
import NLP.Probability.ConditionalDistribution
import NLP.Probability.Distribution 
import NLP.Probability.Observation
import NLP.Semiring.Prob
--}}}


inf = 1e5
tiny = 1e-10

data GraphNode = NodeEdge EdgeVar | NodeNode NodeVar
               deriving (Eq, Ord, Show)

-- mkGraphFromEdges informationAdj grammar edges = 
--     mkGraph ((M.toList newNodeBunch) ++ (M.toList $ (mconcat nodeEdgeBunch))) $ 
--                 (extraEdges ++ concat edgeBunch) 
--     where (nodeNodeBunch, nodeEdgeBunch, edgeBunch) = unzip3 $ do 
--                              edge@(EdgeVar a b c i j k) <- edges
--                              let nodeEdge = (NodeEdge edge, (-(log $ 1 - getProb (backToRule edge) grammar) + inf,0)) 
--                              let nodeNodes =   [(NodeNode $ NodeVar a i k, (0, inf)),
--                                                 (NodeNode $ NodeVar b i j, (0, inf)),
--                                                 (NodeNode $ NodeVar c j k, (0, inf))]
--                              let edges = map (\(n,_) -> (fst nodeEdge, n,  inf))  nodeNodes 
--                              return $  (M.fromList nodeNodes, uncurry M.singleton nodeEdge, edges) 
--           extraEdges = do
--             n1@(NodeNode (NodeVar a i j),_) <- M.toList $ uniqueNodeNode
--             n2@(NodeNode (NodeVar b j' k),_) <- M.toList uniqueNodeNode
--             guard $ j == j'
--             let infAB = informationAdj a b 
--             guard $  (infAB > 1.0)
--             return (fst n1, fst n2, infAB)
--           -- update nodes for 
--           newNodeBunch = foldl (\m (n1,n2,w) -> M.update (\(a,b) -> Just (a,b+ w)) n2 m) uniqueNodeNode extraEdges
--           uniqueNodeNode =  mconcat $ nodeNodeBunch


makeEdgeBunch theta edges = mconcat $ do
  (edge, Prob p) <- edges
  let EdgeVar a b c i j k = edge
  let th = theta edge
  let nodeEdge = (edge, (0, th + tiny))
  return $ uncurry M.singleton nodeEdge 

cacheEdges informationAdj theta edges= ret --trace (show ret) $  ret 
    where ret = do
            let edgeBunch = makeEdgeBunch theta edges
            e1@(EdgeVar a b c i j k, _) <- M.toList edgeBunch
            e2@(EdgeVar x y z i' j' k', _) <- M.toList edgeBunch 
            guard $ k == i'
            let mi = filter (> 1.0) $ map (uncurry informationAdj) [(a, x), (c, x), (a, y), (c, y)]
            guard $ sum mi > 0
            return (fst e2, fst e1, sum mi)

mkGraphFromEdges informationAdj informationPar extraEdges theta edges = --trace (show extraEdges) $  
    mkGraph (M.toList $ newEdgeBunch) extraEdges 
    where 
         edgeBunch = makeEdgeBunch theta edges 
         newEdgeBunch = foldl (\m (n1,n2,w) -> M.update (\(a,b) -> Just (a+w, b)) n1 m) edgeBunch extraEdges
--edgeBunch = 
--           extraEdges2 = do
--             e1@(EdgeVar a b c i j k, _) <- M.toList edgeBunch
--             e2@(EdgeVar x _ _ i' _ k', _) <- M.toList edgeBunch 
--             guard $ ((i == i' && j == k' && b == x) || (j == i' && k ==k' && c == x)) 
--             let mi = informationPar a x
--             guard $ mi > 1.0
--             return (fst e1, fst e2, mi)
         



-- Mutual information

newtype NonTermEvent = NonTermEvent NT
    deriving (Eq, Ord, Binary, Show)

instance Event NonTermEvent where type EventMap NonTermEvent = M.Map

newtype AdjacentEvent = AdjacentEvent (NT, NT)
    deriving (Eq, Ord, Binary, Show)

instance Event AdjacentEvent where type EventMap AdjacentEvent = M.Map


newtype ParentEvent = ParentEvent (NT, NT)
    deriving (Eq, Ord, Binary, Show)

instance Event ParentEvent where type EventMap ParentEvent = M.Map


type Information = NT -> NT -> Double

informationAdj (fobsNT, fobsAdj) nt1 nt2  = 
    if pnt1 < 0.01 || pnt2 < 0.01 then 0.0
    else
    log  ((mle fobsAdj $ AdjacentEvent (nt1, nt2)) / 
          (pnt1 * pnt2))
        where  pnt1 = mle fobsNT (NonTermEvent nt1) 
               pnt2 = mle fobsNT (NonTermEvent nt2)
informationParent (fobsNT, fobsParent) nt1 nt2  = 
    log  ((mle fobsParent $ ParentEvent (nt1, nt2)) / ((mle fobsNT (NonTermEvent nt1) * mle fobsNT (NonTermEvent nt2))) )

collectStats spanMap = (mconcat $ map observation nte,
                        mconcat $ map observation adjacent)
    where 
      nts = filter (\((i,j),_) -> i /= j-1) $ M.toList spanMap 
      nte = map (NonTermEvent . snd) nts
      adjacent = do 
        ((_ ,j), y) <- nts
        ((j',_), z) <- nts
        guard $ j == j'
        return $ AdjacentEvent (y,z)

