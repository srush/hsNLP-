{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-}
module NLP.CNF.CKY where 

--{{{  Imports
import Helpers.Common hiding (Derivation)
import NLP.CNF
import NLP.Semiring.Viterbi
import NLP.Semiring.Derivation
import NLP.Semiring.Prob
import NLP.Semiring.LogProb
import NLP.Language.WordLattice
import Control.Monad.Identity
import qualified NLP.ChartParse.CKY.Inside as P 
import qualified NLP.ChartParse.CKY.Outside as O 
import qualified NLP.ChartParse as CP
import qualified Data.Map as M
import qualified Data.Set as S
import NLP.CNF.LP
import NLP.CNF.Graph
import LP.Format.CPLEX
import LP.Format.Graph
import Debug.Trace
import NLP.Format.TreeBank
import NLP.Semiring
import NLP.Semiring.ViterbiNBestDerivation          
import System.IO.Unsafe (unsafePerformIO)
import LP.DualDecomp
import NLP.Probability.Observation
import NLP.Probability.Distribution hiding (Prob)
import NLP.CNF.Coordinate
import Debug.Trace.Helpers
import System.IO
import System.Process
import Data.Tree
import qualified Data.Text as T
--import qualified Data.Text.IO as TIO
--}}}


type CKYChart semi = CP.Chart (P.CKYSig NT) semi

toParseGrammar :: (WordLattice sent, Symbol sent ~ Word) => 
                  (CP.Span -> Maybe Int -> Production -> semi) -> 
                  (CP.Span -> Word -> [NT]) -> 
                  (CP.Span -> (NT,NT) -> [NT]) -> 
                  ((NT,NT,NT) ->  Bool) ->
                  P.Grammar NT semi sent  
toParseGrammar mkSemi enumerate enumPair exists = 
    P.Grammar { 
           P.enumerate = enumerate,
           P.enumeratePair = enumPair,
           P.exists = exists,
           P.ruleToSemi = (\span split a (b,c) -> mkSemi span (Just split) (BinaryRule a b c)) ,
           P.termToSemi = (\sp a p -> mkSemi sp Nothing (TerminalRule a p)),          
           P.top = TOP
         }

toFixedScoreGrammar :: (WordLattice sent, Symbol sent ~ Word) => 
                    EdgeMap -> 
                    TermMap -> 
                   (Word -> [NT]) -> 
                   (M.Map (NT,NT) (S.Set NT)) ->
                   P.Grammar NT (ViterbiDerivation LogProb (S.Set EdgeVar)) sent  
toFixedScoreGrammar edgemap termmap nts bnts = 
    toParseGrammar mkSemi 
                   (const nts)
                   (\_ nt -> S.toList $ fromJustDef mempty $ M.lookup nt bnts)
                   (const True)
        where mkSemi (i,k) mj rule = 
                  case (rule, mj) of 
                    (BinaryRule a b c, Just j) -> 
                        let edgevar = toEdgeVar (a,b,c, i, j ,k) in 
                        mkViterbi $  Weighted (LogProb $ fromJustDef 0.0 $ getEdgeWeight edgevar edgemap, 
                                           mkDerivation $ S.singleton edgevar) 
                    (TerminalRule a p, Nothing) -> 
                        let edgevar = TermVar a p i in 
                        mkViterbi $  Weighted (LogProb $  fromJustDef 0.0 $  getTermWeight edgevar termmap, 
                                           mkDerivation $ S.singleton edgevar) 

toViterbiGrammar :: (WordLattice sent, Symbol sent ~ Word) => 
                    Theta EdgeVar -> 
                   (Word -> [NT]) -> 
                   (M.Map (NT,NT) (S.Set NT)) ->
                   Maybe (CP.Span -> Maybe NT) ->
                   P.Grammar NT (ViterbiDerivation LogProb (S.Set EdgeVar)) sent  
toViterbiGrammar theta nts bnts correct = toParseGrammar mkSemi
                                    (case correct of  
                                             Nothing -> (\_ -> nts)
                                             Just correct -> (\ sp _ -> maybe [] return  $ correct sp)) 
                                    (case correct of 
                                       Nothing -> (\_ nt -> S.toList $ fromJustDef mempty $ M.lookup nt bnts)
                                       Just correct -> (\ sp _ -> maybe [] return  $ correct sp)) 
                                    (const True)
    where mkSemi (i,k) mj rule =
              case (rule,mj) of 
                (BinaryRule a b c, Just j) -> 
                    let edgevar = toEdgeVar (a,b,c, i, j ,k) in 
                    mkViterbi $  Weighted (LogProb $ theta edgevar  , 
                                           mkDerivation $ S.singleton edgevar) 
                (TerminalRule a p, Nothing) -> 
                    let edgevar = TermVar a p i in 
                    mkViterbi $  Weighted (LogProb $ theta edgevar , 
                                           mkDerivation $ S.singleton edgevar) 
                _ -> undefined

toInsideGrammar :: (WordLattice sent, Symbol sent ~ Word) => 
                  Grammar -> (Word -> [NT]) -> (M.Map (NT,NT) (S.Set NT)) -> P.Grammar NT Prob sent  
toInsideGrammar grammar nts bnts = 
    toParseGrammar (\_ _ -> Prob . flip (getProb) grammar)
                   (\_ -> nts) 
                   (\_ nt -> S.toList $ fromJustDef mempty $ M.lookup nt bnts) 
                   (\(x,y,z) -> S.member x $ fromJustDef mempty $ M.lookup (y,z) bnts) 
                    




-- toLPGrammar :: (WordLattice sent, Symbol sent ~ Word) => 
--                    Grammar -> (Word -> [NT]) -> ((NT,NT) -> [NT]) -> P.Grammar NT (Weighted Prob (DualDerivation Identity EdgeSet)) sent  
-- toLPGrammar grammar nts bnts = 
--     toParseGrammar (\span split rule -> Weighted  (Prob $ getProb rule grammar, mkDualDerivation $ Identity $ createEdgeSemi span split rule))  (\_ -> nts) (\_ -> bnts)  
--         where createEdgeSemi (i,k) (Just j) rule@(BinaryRule a b c) = singleEdgeSet (EdgeVar a b c i j k) 
--               createEdgeSemi (i,_) _ rule@(TerminalRule a p) = singleEdgeSet (TermVar a p i)


-- toCountGrammar :: (WordLattice sent, Symbol sent ~ Word) => 
--                   (CP.Span -> Maybe NT) ->
--                   P.Grammar NT (Derivation CNFObs) sent  
toCountGrammar correct =
   toParseGrammar (\_ _-> mkDerivation . mkRuleObservation)  
        (\ sp _ -> maybe [] return  $ correct sp) 
        (\sp _ -> maybe [] return  $ correct sp)
        (const True)



toParentCountGrammar correct =
   toParseGrammar (\_ _ -> mkDerivation . mkParentObservations)  
        (\ sp _ -> maybe [] return  $ correct sp) 
        (\sp _ -> maybe [] return  $ correct sp)
        (const True)
            where  
              mkParentObservations (TerminalRule _ _) = mempty 
              mkParentObservations (BinaryRule a b c) = mconcat $ map observation [ParentEvent (a,b), ParentEvent (a,c)]  

-- toTempLPGrammar :: (WordLattice sent, Symbol sent ~ Word) => 
--                    Int -> 
--                   (CP.Span -> Maybe NT) ->
--                   P.Grammar NT (Derivation EdgeInfo) sent  
-- toTempLPGrammar n correct = 
--     toParseGrammar (\span split rule -> mkDerivation $ createEdgeSemi span split rule)  (\sp -> maybe [] return  $ correct sp) 
--         where createEdgeSemi (i,k) (Just j) (BinaryRule a b c) = singleEdgeInfo (EdgeVar a b c i j k, 1.0) n 
--               createEdgeSemi (i,_) _ (TerminalRule a p) = singleEdgeInfo (TermVar a p i, 1.0) n

--prune = filter (\(_, c) -> (fromViterbi c) >  Prob 1e-10) 
--pruneLP = filter (\(_, c) -> (getWeight c) >  Prob 1e-10) 

prune ps =
     filter (\(_,p) -> p >= Prob 1e-110 &&  p >= (best/1000000))  ps  
         where
           best = case map (snd) ps of
                [] -> 0.0
                ls -> maximum $ ls

pruneV ps =
     filter (\(_,p) -> (getWeight $ fromViterbi p) >= LogProb (log 1e-110) &&  (getWeight $ fromViterbi p) >= (best-(log 10000))) ps  
         where
           best = case map (getWeight . fromViterbi . snd) ps of
                [] -> 0.0
                ls -> maximum $ ls

-- doParse :: [Word] -> 
doParse sent grammar nts bnts = P.ckyParse sent grammar prune
--doLPParse sent grammar nts bnts = P.ckyParse sent (toLPGrammar grammar nts bnts) pruneLP

doViterbiParse sent theta nts bnts correct = 
    P.ckyParse sent (toViterbiGrammar theta nts bnts correct) pruneV 

instance (Monoid a) => Monoid (Identity a) where 
    mempty = Identity mempty
    mappend (Identity a) (Identity b) = Identity (a `mappend` b)

doCountParse = doRestrictedParse toCountGrammar
doParentCountParse = doRestrictedParse toParentCountGrammar
--doLPParse n = doRestrictedParse (toTempLPGrammar n)

doRestrictedParse mkGrammar (sent, spanMap) = case semi of 
                                 Just s -> s 
                                 Nothing -> mempty -- trace (show sent) mempty
    where (semi, chart) = P.ckyParse (mkSentence sent) (mkGrammar (\sp -> M.lookup sp spanMap) ) id

doManyCountParse  parse ses = mconcat . map parseOne $ zip [1..] ses  
    where parseOne (i,s) = case  parse s of 
              Derivation (Just a) -> a   
              Derivation _ -> mempty -- trace (show (i,s)) $ mempty

-- getGrammar :: IO Grammar
-- getGrammar = decodeFile "grammar.bin"

-- testSent = mkSentence $ map mkWord ["NNP_0", "NNP_0"]
readNT :: String -> IO ([NT])
readNT file = do 
  ntcon <- decodeFile file
  return ntcon

viterbi sent (bnts, wordnts, nts, _) theta =  ret  
    where 
      ret = doViterbiParse (mkSentence sent) theta (\p -> maybe [] S.toList $ M.lookup p wordnts) bnts Nothing -- (Just (\sp -> M.lookup sp spanMap))

simpleViterbi ip@(_,_,_,g) sent = viterbi sent ip (mkTheta g)

extractInside :: (s -> Double) -> CKYChart s -> M.Map (CP.Span, NT) Double  
extractInside getWeight chart = M.fromList $ do 
  (span, (nt, semi)) <- CP.extractItems chart 
  return ((span, nt), getWeight semi) 

-- Coordinate 
mrfEdgePenalty :: Theta EdgeVar -> M.Map (CP.Span, NT) Double -> EdgeVar -> Double 
mrfEdgePenalty theta ntcost edge@(EdgeVar a b c i j k) =
     rho (a, i, k) - rho (b, i, j) - rho (c, j, k) - theta edge    
        where rho (nt, i,j) = fromJustDef 0.0 $ M.lookup ((i,j), nt) ntcost

genericPenalty :: M.Map EdgeVar Double -> EdgeVar -> Double
genericPenalty m edgevar =  fromJustDef 0.0 $ M.lookup edgevar m
          
justEdges = fst . splitDer   
splitDer (Just a, _) = (getBestDerivation  a, getBestScore a)  

testViterbi file = do 
  sents <- readSentenceBunchWithWords "engwords" file
  ip@(_,_,_,g)<- readInsideParams
  let theta = mkTheta g
  return $ [ (length sent, viterbi sent ip theta) | (sent, spanMap) <- sents]

testLPParse = do 
  [(sent,_)] <- readSentenceBunchWithWords "engwords" "onetree"
  ip@(bnts, wordnts, _, gram) <- readInsideParams
  let (edges, g2) = getEdges sent ip
  let lexedges =do (i, pos) <- zip [1..] sent
                   nt <- S.toList $ fromJustDef mempty $ M.lookup pos wordnts
                   return $ TermVar nt pos i 
  let n = (length sent + 1)
  return $ renderCplex $ mkLP n $ edgeSetToInfo gram n  $ map fst edges

testCombLPParse file = do 
  sents <- readSentenceBunchWithWords "engwords" file
  ip@(bnts, wordnts, nts, gram) <- readInsideParams
  (ntobs, adjobs, parobs) <- readGraphParams

  return $ do 
    (sent,_) <- sents
    
    let (edges, g2) = getEdges sent ip
    let nodes = S.toList $ S.fromList $ map (topNode . fst) edges

    let infAdj = informationAdj (finish ntobs,finish adjobs)
    let mrfNodes = cacheNodes (length sent + 1) infAdj nodes
  
    let lexedges =do (i, pos) <- zip [1..] sent
                     nt <- S.toList $ fromJustDef mempty $ M.lookup pos wordnts
                     return $ TermVar nt pos i 
  --return (show lexedges)
    let n = (length sent + 1)
  --return (nodes `seq` [])
    return $ (mkSentence sent, n, renderCplex $ mkCombinedLP False n mrfNodes {-mkLP n-} $ edgeSetToInfo gram n $ (map fst edges ++ lexedges))
 
writeLP file = do 
  parses <- testCombLPParse file
  mapM doOne parses
  where doOne (sent, n, lp) = do 

            h <- openFile ("/tmp/tmp.cplex") WriteMode 
            hPutStr h (lp++"\n")
            hFlush h
            hClose h
            wordTable <- decodeFile "engwords"

            system ("glpsol --cpxlp --max -o /tmp/tmp.sol /tmp/tmp.cplex ") 
            --system ("lp_solve -ia -max -fmps /tmp/tmp.mps > /tmp/tmp.sol")
            contents <- readFile "/tmp/tmp.sol"
            let vars = parseVars wordTable $ T.lines $ last $ T.split (T.pack "Actual values of the variables:\n") $ T.pack contents
            res <- decodeParseVar sent vars
            return $ printTree $ spanSetToTree (justEdges res) n
            --return $ printTree $ spanSetToTree (S.fromList $ map fst vars) n
                                                                            

decodeParseVar sent parsevars  = do
  let (emap, tmap) = (mkEdgeMap parsevars, mkTermMap parsevars)
  ip@(bnts, wordnts, nts, gram) <- readInsideParams 
  let g = toFixedScoreGrammar emap tmap (\p -> maybe [] S.toList $ M.lookup p wordnts) bnts
  return $ P.ckyParse sent g pruneV 

readInsideParams = do
  p <- decodeFile "model/outmodel"
  bnts <- decodeFile "ntpairs"
  wordnts <- decodeFile "wordnts"
  nts <- readNT "nonterms"
  let gram = Grammar $ estimate p
  return (bnts::(M.Map (NT,NT) (S.Set NT) ), wordnts::(M.Map Word (S.Set NT)), nts, gram) 

readGraphParams = do 
  (ntobs, ami) <- decodeFile "model/outMI"
  mip <- decodeFile "model/outMIP"
  return (ntobs :: Counts NonTermEvent, ami::Counts AdjacentEvent, mip:: Counts ParentEvent)
 
getEdges sent (bnts, wordnts, nts, gram) = -- trace (show nts) $ trace (show chart) $  
    (S.toList $ S.fromList $ map (\(ed,w)-> (toEdgeVar ed, w/a)) $ concat extras, gram) 
    where 
      g2 = toInsideGrammar gram (\l -> nts) bnts
      (Just a, chart) = doParse (mkSentence sent) g2 (\l -> nts) bnts 
      (outchart, extras) = O.ckyOutside (mkSentence  sent) chart g2 (\semi -> (semi / a) > 1e-5)         

constructGraph = do 
  [(sent,_)] <- readSentenceBunchWithWords "engwords" "onetree"  
  (ntobs, adjobs, parobs) <- readGraphParams
  ip <- readInsideParams
  let (edges,g2) = getEdges sent ip
  let infAdj =  informationAdj (finish ntobs,finish adjobs) -- trace (show edges) $ 
  let infPar =  informationParent (finish ntobs,finish parobs)
  let mrfEdges = cacheEdges infAdj (mkTheta g2) edges
  --return $ mkGraphFromEdges infAdj g2 edges
  -- putStr $ render $ writeGraph $ mkGraphFromEdges infAdj g2 edges
  return $ solveGraph 0 $ mkGraphFromEdges infAdj infPar mrfEdges (mkTheta g2) edges

testModels = do 
  (a,b):: (Counts NonTermEvent, Counts AdjacentEvent) <- decodeFile "model/outMI"
  return (a,b) 

testInfor = do
  nts <- readNT "nonterms"  
  (a, b) <- testModels
  let (a', b') = (finish a, finish b)
  let p = do 
        nta <- nts
        ntb <- nts
        guard $ (mle b' $ AdjacentEvent (nta, ntb)) > 0.0 
        let inf = informationAdj (a', b') nta ntb
        guard $ inf > 1.0
        return (nta, ntb, inf)
  return p

-- testDualDecomp = do 
--   [(sent,_)] <- readSentenceBunch "onetree"  
--   (ntobs, adjobs, parobs) <- readGraphParams
--   ip@(_,_,_,g) <- readInsideParams
--   let (edges,g2) = getEdges sent ip
--   let infAdj = informationAdj (finish ntobs,finish adjobs)
--   let infPar = informationParent (finish ntobs,finish parobs)
--   let theta = mkTheta g 
--   let mrfEdges = cacheEdges infAdj theta edges
--   let original = justEdges $ viterbi sent ip theta


--   let bbox1 = Slave (\lambda i -> return $ S.filter isFullEdge $ justEdges $ viterbi sent ip (combine theta lambda (+) 1.0))
--   let bbox2 = Slave (\lambda i -> trace (show i) $ S.fromList `liftM` (solveGraph i $ mkGraphFromEdges infAdj infPar mrfEdges (\e -> fromJustDef 0.0 $ M.lookup e lambda) edges))
--   master <- run original takeStep 500 $ initialize theta (bbox1, bbox2)
--   finalMAP master 
--     where
--       run original fn n start = foldM doOne start [1..n]
--           where doOne a i = trace (show $ m1 ) $ 
--                             trace (show $ m2) $ fn a
--                     where (m1, m2) = unsafePerformIO $ bothMAP a

computeRealCKY theta lambda edges = edgeSum + sum good - sum bad
    --(trace $ ("BAD:" ++ (show $ sum bad))) $ 
    where 
      
      good = map snd $ filter (\(a,_) -> S.member a nodeSet ) $ M.toList lambda
      bad  = map snd $ filter (\(a,_) -> not $ S.member a nodeSet ) $ M.toList lambda
      lambdam = (\e -> fromJustDef 0.0 $ M.lookup e lambda)
      edgeSum = sum $ map theta $ S.toList edges
      nodeSet = S.map topNode edges

testDualDecomp = do 
  [(sent,_)] <- readSentenceBunchWithWords "engwords" "onetree"  
  (ntobs, adjobs, parobs) <- readGraphParams
  ip@(_,_,_,g) <- readInsideParams
  let (edges,g2) = getEdges sent ip
  let nodes = map (topNode . fst) edges
  let theta = mkTheta g 
  let original = justEdges $ viterbi sent ip theta


  let infAdj = informationAdj (finish ntobs,finish adjobs)
  --let infPar = informationParent (finish ntobs,finish parobs)
  let mrfNodes = cacheNodes (length sent + 1) infAdj  nodes

  let bbox1 = Slave (\lambda i -> do
                       let ps = map snd $ M.toList lambda
                       let low = if null ps then 0.0 else minimum $ ps
                       let (edges, blah) = splitDer $ 
                                viterbi sent ip 
                                (\e ->  (theta e) + (-low) + (fromJustDef 0.0 $ M.lookup (topNode e) lambda))
                       let obj = computeRealCKY theta lambda edges
                       return $ -- trace ("NOT ONE:" ++ show blah) $ trace ("ONE: "++(show obj)) $ 
                                  (obj, S.map topNode $ S.filter isFullEdge $ edges))
                                

  let bbox2 = Slave (\lambda i -> do
                       let ps = map snd $ M.toList lambda
                       let low = if null ps then 0.0 else minimum $ ps

                       let theta = (\e -> 2 * (fromJustDef 0.0 $ M.lookup e lambda))
                       let graph = mkGraphFromNodes mrfNodes theta nodes 
                       (_, sol) <- solveGraph i graph
                       let realObj = {-trace ("LAMBDA:"++  show lambda) $ -} computeRealObj mrfNodes lambda sol
                       return $ {-trace ("TWO: "++(show realObj))$-} (realObj, S.fromList sol) 
                    )

  master <- run original takeStep 200 $  initialize (bbox1::Slave NodeVar, bbox2::Slave NodeVar)
--  finalMAP master 
  return $ map (\(a,b) -> a + b) $ obj master

    where
      run original fn n start = foldM doOne start [1..n]
          where doOne a i = fn a --trace (show $ m1 ) $ 
                            --trace (show $ m2) $ fn a
                    where (m1, m2) = unsafePerformIO $ bothMAP a


-- testCoordinate = do 
--   [(sent,_)] <- readSentenceBunch "onetree"  
--   (ntobs, adjobs, parobs) <- readGraphParams
--   ip@(_,_,_,g) <- readInsideParams
--   let (edges,g2) = getEdges sent ip
--   let infAdj = informationAdj (finish ntobs, finish adjobs)
--   let infPar = informationParent (finish ntobs, finish parobs)
--   let theta = mkTheta g 
--   let mrfEdges = cacheEdges infAdj theta edges
  
--   let original = justEdges $ viterbi sent ip theta

--   let bbox1 = Updater (\lambda _ -> return $ mrfEdgePenalty theta $ extractInside (convertToDouble . getBestScore) $ snd $ viterbi sent ip $ penalize theta lambda)
--   let bbox2 = Updater (\lambda i -> do 
--                          (on,res) <- solveGraph i $ mkGraphFromEdges infAdj infPar mrfEdges lambda edges
                         
--                          return $ trace (show on) $ genericPenalty $ M.fromList res)
                                    
                                    
--   master <- run 5 $ initCoordinate (bbox1, bbox2)
--   master `seq` return () 
--     where
--       run n start = foldM doOne start [1..n]          
--           where doOne a i = runCoordOne a

--addTop (s, b) = (s++[EndWord], M.insert (n+1, n+2) termTop $ M.insert (1,n+2) top b) 
--    where n = length s 

readSentenceBunchWithWords words file = do
  sents <- readSentenceBunch file
  wordTable <- decodeFile words
  return $ do
    (s, b) <- sents
    return  $ (map (mkWord wordTable) s, b)

spanSetToTree spanset n = Node w [n1] 
    where (Node w [n1,_]) = getWithSpan 1 n 
      
          getWithSpan i k = 
              if i +1 /= k then
                  head $ do 
                    EdgeVar a _ _ i' j' k'<- all
                    guard (i' == i && k' == k)
                    return $ Node (show a) $ [getWithSpan i j', getWithSpan j' k]  
              else
                  head $ do 
                    TermVar a b i' <- all
                    guard (i' == i)
                    return $ Node (show a) $ [Node (show b) []]
                    
          all = S.toList spanset
              
printTree (Node w []) = 
    (text w) 
printTree (Node w rest) =      
    (lparen <> (text w))
    <+>
    (hsep $ map printTree rest) <> rparen
