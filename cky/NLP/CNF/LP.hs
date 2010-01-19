{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module NLP.CNF.LP where

--{{{  Imports
import Helpers.Common 
import LP.Format.CPLEX
import NLP.CNF
import qualified Data.Map as M
import Text.Printf
import qualified Data.Set as S 
import Debug.Trace
import qualified  Data.Text as T
--}}}

data EdgeVar = EdgeVar NT NT NT Int Int Int | 
               TermVar NT Word Int 
               deriving (Eq, Ord, Show)

toEdgeVar (a,b,c,i,j,k)= EdgeVar a b c i j k 

isFullEdge (EdgeVar _ _ _ _ _ _) = True 
isFullEdge _ = False 

mkEdgeVar (EdgeVar a b c i j k) = Var $ printf "rul(%s,%s,%s,%d,%d,%d) " (clean $ show a) (clean $ show b) (clean $ show c) i j k
mkEdgeVar (TermVar a p i ) = Var $ printf "trm(%s,%s,%d) " (clean $ show a) (clean $ show p) i 

data NodeVar = NodeVar NT Int Int
            deriving  (Eq, Ord, Show)

mkNodeVar (NodeVar a i k) = Var $ printf "nod(%s,%d,%d) " (clean $ show a) i k 

mkCorVar a b = Var $ (mkCorName a b ++ " ")
mkCorName (NodeVar b j k) (NodeVar a i _) =  printf "cor(%s,%s,%d,%d,%d)" (clean $ show a) (clean $ show b) i j k

topNode (EdgeVar a b c i j k) = NodeVar a i k 
topNode (TermVar a _ i) = NodeVar a i (i+1) 

mkNodeName (NodeVar nt i j) = printf "nod(%s,%d,%d)" (clean $ show nt) i j 
 
newtype EdgeSet = EdgeSet (S.Set EdgeVar)
             deriving (Monoid, Show)

singleEdgeSet = EdgeSet . S.singleton

edgeSetSize (EdgeSet set) = S.size set

edgeSetToInfo grammar n s = mconcat $ map (\edge -> singleEdgeInfo (edge, log $ getProb (backToRule edge) grammar) n) s 


-- TODO - add log
mkTheta grammar edgevar = log $ getProb (backToRule edgevar) grammar 

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

newtype EdgeInfo = EdgeInfo ([(EdgeVar, Double)], S.Set NodeVar, M.Map Int [EdgeVar], EdgeStore) 
    deriving (Show)

instance Monoid EdgeInfo where 
    mempty = EdgeInfo mempty
    mappend (EdgeInfo (a,b,c,d)) (EdgeInfo (a',b',c',d')) = EdgeInfo (a `mappend` a', 
                                                                      b `mappend` b',
                                                                      M.unionWith mappend c c',
                                                                      d `mappend` d'
                                                                     )

mkLP n e@(EdgeInfo (edges, final, bottom, EdgeStore store)) = 
    
    LP (Objective Max (Formula $ map (\(e,p) -> (p, mkEdgeVar e) ) edges)) 
       (mkFinalConstraint final : 
        (map mkLexConstraint $ M.toList bottom) ++
        (concatMap (mkEdgeConstraint n) $ M.toList store))
       (map ((\edge -> Bound 0 (mkEdgeVar edge) 1).fst) edges)
       []

mkCombinedLP isIlp n nodeCor e@(EdgeInfo (edges, final, bottom, EdgeStore store))  = 
    --trace (show e) $ 
    LP (Objective Max (Formula $ (map (\(e,p) -> (p, mkEdgeVar e) ) edges ++
                                  (map (\(n1, n2, c) -> (c, mkCorVar n1 n2)) nodeCor)
                                 )
                      )) 
       (mkFinalConstraint final : 
        (map mkLexConstraint $ M.toList bottom) ++
        (concatMap (mkEdgeConstraint n) $ M.toList store) ++ 
        (concatMap mkNodeCor nodeCor) 
       )
       (if isIlp then [] else map ((\edge -> Bound 0 (mkEdgeVar edge) 1).fst) edges)
       (if isIlp then (map (mkEdgeVar . fst) edges) else [])
  
newtype EdgeStore = EdgeStore (M.Map NodeVar EdgeHolder)
    deriving Show

instance Monoid EdgeStore where 
    mempty = EdgeStore M.empty
    mappend (EdgeStore a) (EdgeStore b) = EdgeStore $ M.unionWith mappend a b

mkEdgeStore = EdgeStore . M.fromList 

singleEdgeInfo (rule,p) n = 
     case rule of 
       (EdgeVar _ _ _ i j k) -> if i == 1 && k == n then EdgeInfo ([(rule,p)], S.singleton $ topNode rule, lex i j k, singleEdgeStore rule) 
                                else EdgeInfo ([(rule,p)], mempty, lex i j k, singleEdgeStore rule) 
       (TermVar _ _ i) ->  EdgeInfo ([(rule,p)], mempty, M.singleton i [rule], singleEdgeStore rule)
    where lex i j k = mempty --mconcat [if j == i + 1 then M.singleton i [rule]  else mempty, 
                      --   if k == j +1  then M.singleton j [rule] else mempty] 


singleEdgeTop rule@(EdgeVar a b c i j k) = EdgeStore $ M.fromList [(NodeVar b i j, EdgeHolder [] [rule]),
                                                                   (NodeVar c j k, EdgeHolder [] [rule])]

singleEdgeStore rule@(EdgeVar a b c i j k) = EdgeStore $ M.fromList $ catMaybes [
                                              Just (NodeVar a i k, EdgeHolder [rule] []) ,
                                              --if i == j -1 then Nothing else 
                                                  Just (NodeVar b i j, EdgeHolder [] [rule]) ,
                                              --if j == k -1 then Nothing else 
                                                  Just (NodeVar c j k, EdgeHolder [] [rule]) ]

singleEdgeStore rule@(TermVar a p i) = EdgeStore $ M.fromList [(NodeVar a i (i+1), EdgeHolder [rule] [])]

mkEdgeConstraint l (n, EdgeHolder top bottom) = 
    catMaybes
    [--if i == k -1 then Nothing 
      Just $ Constraint (nodeName ++"_UP") (Formula ((-1, nodeVar) : up)) OEQ 0,
     if i == 1 && k == l then Nothing 
     else Just $ Constraint (nodeName ++ "_DOWN") (Formula ((-1, nodeVar) : down)) OEQ 0]
    where 
      (NodeVar _ i k) = n
      nodeVar = mkNodeVar n
      nodeName = mkNodeName n
      up = vars top
      down = vars bottom
      vars = map (\t -> (1, mkEdgeVar t))

mkFinalConstraint final = Constraint "final" (Formula form) OEQ 1 
    where form = map (\t -> (1, mkNodeVar t)) $ S.toList final

mkLexConstraint (i,bottom) = Constraint ("lex" ++  show i) (Formula form) OEQ 1 
    where form = map (\t -> (1, mkEdgeVar t)) bottom

mkNodeCor (n1, n2, cor) = [Constraint (mkCorName n1 n2 ++ "_1") (Formula [(-1,mkNodeVar n1), (1,mkCorVar n1 n2)]) OLTE 0,
                           Constraint (mkCorName n1 n2 ++ "_2") (Formula [(-1,mkNodeVar n2), (1,mkCorVar n1 n2)]) OLTE 0]




-- 
-- Parse
--

parseVars wordtable lines = do
  node <- lines
  guard $ T.isPrefixOf (T.pack "trm") node || T.isPrefixOf (T.pack "rul") node
  let (argStr, valStr) =  T.break (==  ')') $ T.dropWhile ((==) '(') $ T.dropWhile ((/=) '(') node

  let val = (read $ T.unpack $ T.dropWhile ((==) ')') valStr) ::Double 
  let args = T.split (T.pack ",") argStr

  if T.isPrefixOf (T.pack "trm") node then
      let [t, w , i] = args in 
      return $ (TermVar (mkNT $ T.unpack $ unclean t) (mkWord wordtable $ T.unpack $ unclean w)(read $ T.unpack i),
                val)
   else
      let [a,b,c,i,j,k] = args in 
      return $ (EdgeVar (mkNT $ T.unpack $ unclean a)  (mkNT $ T.unpack $ unclean b)(mkNT $ T.unpack $ unclean c) 
                 (read $ T.unpack i) (read $ T.unpack j) (read $ T.unpack k),
                val)

mkTermMap edges = TermMap $ M.fromListWith (M.unionWith M.union) $ do
  (TermVar a w i, p) <- edges
  return (i, M.singleton w (M.singleton a p))
 

mkEdgeMap edges = EdgeMap $ M.fromListWith (M.unionWith M.union) $ do
  (EdgeVar a b c i j k, p) <- edges
  return ((i,j,k), M.singleton (b,c) (M.singleton a p))
 
getEdgeWeight (EdgeVar a b c i j k) (EdgeMap em) = do
    m <- M.lookup (i, j, k) em
    m' <-M.lookup (b,c) m
    M.lookup a m'
getTermWeight (TermVar a w i) (TermMap tm) = do
    m <- M.lookup i tm
    m' <- M.lookup w m
    M.lookup a m'

newtype EdgeMap = EdgeMap (M.Map (Int, Int, Int) (M.Map (NT, NT) (M.Map NT Double)))

newtype TermMap = TermMap (M.Map Int (M.Map Word (M.Map NT Double)))