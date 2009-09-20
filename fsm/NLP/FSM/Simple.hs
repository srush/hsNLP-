{-# LANGUAGE TypeFamilies #-}
module NLP.FSM.Simple where 
import qualified Data.Map as M 
import NLP.FSM
import NLP.Semiring
import Data.Function (on)
import Data.Graph.Inductive
import Control.Exception
import Data.Maybe (maybeToList)
import Control.Monad (liftM)


type SimpleGraph node edge semi = M.Map node (M.Map edge (node, semi)) 

data GraphWFSM node symbol semiring = GraphWFSM {
      initial :: [(GraphState node symbol semiring, semiring)],
      simpleGraph :: SimpleGraph node symbol semiring

}

data GraphState node symbol semi = GraphState {
      stateid :: node, 
      nextState ::  symbol -> [(GraphState node symbol semi, semi)],
      final :: Bool
}

instance (Eq node) => Eq (GraphState node edge semi)  where 
    (==) = (==) `on` stateid 

instance (Ord node) => Ord (GraphState node edge semi)  where 
    compare = compare `on` stateid 

instance (Show node) => Show (GraphState node edge semi)  where 
    show st = show (stateid st) ++ (if final st then "F" else "")


instance (Ord node, Ord edge, Show node, Show edge, Semiring semi) => WFSM (GraphWFSM node edge semi) where 
    type State (GraphWFSM node edge semi) = GraphState node edge semi
    initialState = initial
    

instance (Ord node, Show node, Ord edge, Show edge, Semiring semi) => FSMState (GraphState node edge semi) where 
    type FSMSymbol (GraphState node edge semi) = edge
    type FSMSemiring (GraphState node edge semi) = semi
    next = nextState
    isFinal = final



makeWFSM :: (Ord node, Ord symbol, Semiring semiring) => 
            node -> 
            node -> 
            Maybe symbol ->
            [(node, [(symbol, (node, semiring))])] -> 
            GraphWFSM node symbol semiring

makeWFSM initial final epsilon edges = 
    GraphWFSM  (expandState initial one epsilon) graph
    where 
          mkState id = GraphState {
                      stateid = id,
                      final = (id == final),
                      nextState = next id 
                    }
          next id sym =
            case  M.lookup sym ((M.!) graph id) of 
              Nothing -> []
              Just (out, semi) -> expandState out semi epsilon
          graph = M.fromList $ map (\(a, b) -> (a, M.fromList b)) edges 

          expandState st semi mepsilon =
              maybe [(mkState st,semi)] expand' mepsilon
                  where expand' epsilon = 
                            (mkState st,semi) :  
                            case M.lookup epsilon ((M.!) graph st) of 
                              Nothing -> [] 
                              Just (out, semi') -> expandState out (semi `times` semi') (Just epsilon)
                

toGraph :: (Ord state) => GraphWFSM state symbol semi -> Gr state symbol
toGraph fsa = gr 
    where (gr, _) = mkMapGraph (M.keys sg) 
                    (concatMap (\(node, edges) -> map (\(edge, (outnode,_)) -> 
                                                       assert (M.member outnode sg) $ (node,outnode, edge)) $ M.toList edges) $ M.toList sg)
          sg = simpleGraph fsa


instance (Ord node, Show node, Show symbol, Show semi) => Show (GraphWFSM node symbol semi) where
    show = show . toGraph 


--test = 
--  let fsa = makeWFSM 1 3 [(1, [("hello", (2, Boolean True))]), (2, [("bye",(3, Boolean True) ), ("yo", (3, Boolean True))]), (3, []) ]
 -- in toGraph fsa