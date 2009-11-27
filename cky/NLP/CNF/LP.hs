{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module NLP.CNF.LP where

--{{{  Imports
import Helpers.Common 
import LP.Format.CPLEX
import NLP.CNF
import qualified Data.Map as M
import Text.Printf
import qualified Data.Set as S 
--}}}

data EdgeVar = EdgeVar NT NT NT Int Int Int | 
               TermVar NT POS Int 
               deriving (Eq, Ord, Show)




toEdgeVar (a,b,c,i,j,k)= EdgeVar a b c i j k 

mkEdgeVar (EdgeVar a b c i j k) = Var $ printf "rul(%s,%s,%s,%d,%d,%d) " (show a) (show b) (show c) i j k
mkEdgeVar (TermVar a p i ) = Var $ printf "trm(%s,%s,%d) " (show a) (show p) i 

data NodeVar = NodeVar NT Int Int
            deriving  (Eq, Ord, Show)

mkNodeName (NodeVar nt i j) = printf "nod(%s,%d,%d)" (show nt) i j 
 
newtype EdgeSet = EdgeSet (S.Set EdgeVar)
             deriving (Monoid, Show)

singleEdgeSet = EdgeSet . S.singleton

edgeSetSize (EdgeSet set) = S.size set

edgeSetToInfo grammar n s = mconcat $ map (\edge -> singleEdgeInfo (edge, log $ getProb (backToRule edge) grammar) n) s 


-- TODO - add log
mkTheta grammar edgevar = getProb (backToRule edgevar) grammar 

backToRule (EdgeVar a b c i j k) = BinaryRule a b c
backToRule (TermVar a p i) = TerminalRule a p
data EdgeHolder = EdgeHolder { 
      above :: [EdgeVar],
      below :: [EdgeVar]
    }
  deriving Show

instance Monoid EdgeHolder where 
    mempty = EdgeHolder [] []
    mappend (EdgeHolder a b) (EdgeHolder a' b')= 
        EdgeHolder (a ++ a') (b ++ b') 

newtype EdgeInfo = EdgeInfo ([(EdgeVar, Double)], [EdgeVar], M.Map Int [EdgeVar], EdgeStore) 
    deriving (Show)

instance Monoid EdgeInfo where 
    mempty = EdgeInfo mempty
    mappend (EdgeInfo (a,b,c,d)) (EdgeInfo (a',b',c',d')) = EdgeInfo (a `mappend` a', 
                                                                      b `mappend` b',
                                                                      M.unionWith mappend c c',
                                                                      d `mappend` d'
                                                                     )

mkLP (EdgeInfo (edges, final, bottom, EdgeStore store)) = 
    LP (Objective Max (Formula $ map (\(e,p) -> (p, mkEdgeVar e) ) edges)) 
       (mkFinalConstraint final : 
        (map mkLexConstraint $ M.toList bottom) ++
        (map mkEdgeConstraint $ M.toList store))
       (map ((\edge -> Bound 0 (mkEdgeVar edge) 1).fst) edges)
  
newtype EdgeStore = EdgeStore (M.Map NodeVar EdgeHolder)
    deriving Show

instance Monoid EdgeStore where 
    mempty = EdgeStore M.empty
    mappend (EdgeStore a) (EdgeStore b) = EdgeStore $ M.unionWith mappend a b

mkEdgeStore = EdgeStore . M.fromList 

singleEdgeInfo (rule,p) n = 
     case rule of 
       (EdgeVar _ _ _ i j k) -> if i == 1 && k == n then EdgeInfo ([(rule,p)], [rule], lex i j k, singleEdgeTop rule) 
                                else EdgeInfo ([(rule,p)], [], lex i j k, singleEdgeStore rule) 
       (TermVar _ _ i) ->  EdgeInfo ([(rule,p)], [], M.singleton i [rule], singleEdgeStore rule)
    where lex i j k = mconcat [if j == i + 1 then M.singleton i [rule]  else mempty, 
                         if k == j +1  then M.singleton j [rule] else mempty] 


singleEdgeTop rule@(EdgeVar a b c i j k) = EdgeStore $ M.fromList [(NodeVar b i j, EdgeHolder [] [rule]),
                                                                   (NodeVar c j k, EdgeHolder [] [rule])]

singleEdgeStore rule@(EdgeVar a b c i j k) = EdgeStore $ M.fromList $ catMaybes [
                                              Just (NodeVar a i k, EdgeHolder [rule] []) ,
                                              if i == j -1 then Nothing else Just (NodeVar b i j, EdgeHolder [] [rule]) ,
                                              if j == k -1 then Nothing else Just (NodeVar c j k, EdgeHolder [] [rule]) ]
singleEdgeStore rule@(TermVar a p i) = EdgeStore $ M.fromList [(NodeVar a i (i+1), EdgeHolder [rule] [])]

mkEdgeConstraint (n, EdgeHolder top bottom) = Constraint (mkNodeName n) (Formula form) OEQ 0
    where form =  map (\t -> (1,  mkEdgeVar t)) top ++  
                  map (\t -> (-1, mkEdgeVar t)) bottom

mkFinalConstraint final = Constraint "final" (Formula form) OEQ 1 
    where form = map (\t -> (1, mkEdgeVar t)) final

mkLexConstraint (i,bottom) = Constraint ("lex" ++  show i) (Formula form) OEQ 1 
    where form = map (\t -> (1, mkEdgeVar t)) bottom
