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

--}}}


inf = 1e5

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


mkGraphFromEdges informationAdj grammar edges = trace (show edges) $  
    mkGraph (M.toList $ newEdgeBunch) extraEdges 
    where edgeBunch = mconcat $ do 
                          edge@(EdgeVar a b c i j k) <- edges
                          let nodeEdge = (edge, (-(log $ 1 - getProb (backToRule edge) grammar),0))
                          return $ uncurry M.singleton nodeEdge 
          extraEdges = do
            e1@(EdgeVar a b c i j k, _) <- M.toList edgeBunch
            e2@(EdgeVar x y z i' j' k', _) <- M.toList edgeBunch 
            guard $ k == i'
            let mi = filter (> 1.0) $ map (uncurry informationAdj) [(a, x), (c, x), (a, y), (c, y)]
            guard $ sum mi > 0
            return (fst e1, fst e2, sum mi)
          informative (a,b) = informationAdj a b > 1.0
          newEdgeBunch = foldl (\m (n1,n2,w) -> M.update (\(a,b) -> Just (a,b+ w)) n2 m) edgeBunch extraEdges



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
    log  ((mle fobsAdj $ AdjacentEvent (nt1, nt2)) / ((mle fobsNT (NonTermEvent nt1) * mle fobsNT (NonTermEvent nt2))) )

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

