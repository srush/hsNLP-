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
--}}}


type CKYChart semi = CP.Chart (P.CKYSig NT) semi

toParseGrammar :: (WordLattice sent, Symbol sent ~ POS) => 
                  (CP.Span -> Maybe Int -> Production -> semi) -> 
                  (CP.Span -> POS -> [NT]) -> 
                  (CP.Span -> (NT,NT) -> [NT]) -> 
                  ((NT,NT,NT) ->  Bool) ->
                  P.Grammar NT semi sent  
toParseGrammar mkSemi enumerate enumPair exists = 
    P.Grammar { 
           P.enumerate = enumerate,
           P.enumeratePair = enumPair,
           P.exists = exists,
           P.ruleToSemi = (\span split a (b,c) -> mkSemi span (Just split) (BinaryRule a b c)) ,
           P.termToSemi = (\sp a p -> mkSemi sp Nothing (TerminalRule a p))          
         }

toViterbiGrammar :: (WordLattice sent, Symbol sent ~ POS) => 
                    Theta EdgeVar -> 
                   (POS -> [NT]) -> 
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

toInsideGrammar :: (WordLattice sent, Symbol sent ~ POS) => 
                  Grammar -> (POS -> [NT]) -> (M.Map (NT,NT) (S.Set NT)) -> P.Grammar NT Prob sent  
toInsideGrammar grammar nts bnts = 
    toParseGrammar (\_ _ -> Prob . flip (getProb) grammar)
                   (\_ -> nts) 
                   (\_ nt -> S.toList $ fromJustDef mempty $ M.lookup nt bnts) 
                   (\(x,y,z) -> S.member x $ fromJustDef mempty $ M.lookup (y,z) bnts) 
                    

-- toLPGrammar :: (WordLattice sent, Symbol sent ~ POS) => 
--                    Grammar -> (POS -> [NT]) -> ((NT,NT) -> [NT]) -> P.Grammar NT (Weighted Prob (DualDerivation Identity EdgeSet)) sent  
-- toLPGrammar grammar nts bnts = 
--     toParseGrammar (\span split rule -> Weighted  (Prob $ getProb rule grammar, mkDualDerivation $ Identity $ createEdgeSemi span split rule))  (\_ -> nts) (\_ -> bnts)  
--         where createEdgeSemi (i,k) (Just j) rule@(BinaryRule a b c) = singleEdgeSet (EdgeVar a b c i j k) 
--               createEdgeSemi (i,_) _ rule@(TerminalRule a p) = singleEdgeSet (TermVar a p i)


-- toCountGrammar :: (WordLattice sent, Symbol sent ~ POS) => 
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

-- toTempLPGrammar :: (WordLattice sent, Symbol sent ~ POS) => 
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

-- doParse :: [POS] -> 
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
                                 Nothing -> trace (show sent) mempty
    where (semi, chart) = P.ckyParse (mkSentence $ map mkPOS sent) (mkGrammar (\sp -> M.lookup sp spanMap) ) id

doManyCountParse  parse ses = mconcat . map parseOne $ zip [1..] ses  
    where parseOne (i,s) = case  parse s of 
              Derivation (Just a) -> a   
              Derivation _ -> trace (show (i,s)) $ mempty

-- getGrammar :: IO Grammar
-- getGrammar = decodeFile "grammar.bin"

testSent = mkSentence $ map mkPOS ["NNP_0", "NNP_0"]

readNT file = do 
  ntcon <- readFile "nonterm"
  return $ map mkNT $ lines ntcon


viterbi sent (bnts, bpos, nts, _) theta =  ret  
    where 
      ret = doViterbiParse (mkSentence $ map mkPOS sent) theta (\p -> maybe [] S.toList $ M.lookup p bpos) bnts Nothing -- (Just (\sp -> M.lookup sp spanMap))

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
          
justEdges (Just a, _) = getBestDerivation  a  

testViterbi = do 
  [(sent, spanMap)] <- readSentenceBunch "onetree"
  ip@(_,_,_,g)<- readInsideParams
  let theta = mkTheta g
  return $ fst $ viterbi sent ip theta

testLPParse = do 
  [(sent,_)] <- readSentenceBunch "onetree"
  ip@(bnts, bpos, nts, gram) <- readInsideParams
  let (edges, g2) = getEdges sent ip
  let lexedges =do (i, pos) <- zip [1..] $ map mkPOS sent
                   nt <- S.toList $ fromJustDef mempty $ M.lookup pos bpos
                   return $ TermVar nt pos i 
  return $ renderCplex $ mkLP $ edgeSetToInfo gram (length sent + 1) $ map fst edges
  
writeLP = do 
  lp <- testLPParse
  h <- openFile ("/tmp/tmp.cplex") WriteMode 
  hPutStr h (lp++"\n")
  hFlush h
  hClose h

  system ("glpsol --cpxlp --max --wfreemps /tmp/tmp.mps /tmp/tmp.cplex") 
  system ("lp_solve -ia -max -fmps /tmp/tmp.mps > /tmp/tmp.sol")
  contents <- readFile "/tmp/tmp.sol"
  return contents


readInsideParams = do
  p <- decodeFile "model/outmodel"
  bnts <- decodeFile "ntpairs"
  bpos <- decodeFile "ntpos"
  nts <- readNT "nonterm"
  let gram = Grammar $ estimate p
  return (bnts::(M.Map (NT,NT) (S.Set NT) ), bpos::(M.Map POS (S.Set NT)), nts, gram) 

readGraphParams = do 
  (ntobs, ami) <- decodeFile "model/outMI"
  mip <- decodeFile "model/outMIP"
  return (ntobs :: Counts NonTermEvent, ami::Counts AdjacentEvent, mip:: Counts ParentEvent)
 
getEdges sent (bnts, bpos, nts, gram) =
    (S.toList $ S.fromList $ map (\(ed,w)-> (toEdgeVar ed, w/a)) $ concat extras, gram) 
    where 
      g2 = toInsideGrammar gram (\l -> nts) bnts
      (Just a, chart) = doParse (mkSentence $ map mkPOS sent) g2 (\l -> nts) bnts 
      (outchart, extras) = O.ckyOutside (mkSentence $ map mkPOS sent) chart g2 (\semi -> (semi / a) > 1e-5)         

constructGraph = do 
  [(sent,_)] <- readSentenceBunch "onetree"  
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
  nts <- readNT "nonterm"  
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

testDualDecomp = do 
  [(sent,_)] <- readSentenceBunch "onetree"  
  (ntobs, adjobs, parobs) <- readGraphParams
  ip@(_,_,_,g) <- readInsideParams
  let (edges,g2) = getEdges sent ip
  let infAdj = informationAdj (finish ntobs,finish adjobs)
  let infPar = informationParent (finish ntobs,finish parobs)
  let theta = mkTheta g 
  let mrfEdges = cacheEdges infAdj theta edges
  let original = justEdges $ viterbi sent ip theta


  let bbox1 = Slave (\lambda i -> return $ S.filter isFullEdge $ justEdges $ viterbi sent ip (combine theta lambda (+) 1.0))
  let bbox2 = Slave (\lambda i -> trace (show i) $ S.fromList `liftM` (solveGraph i $ mkGraphFromEdges infAdj infPar mrfEdges (\e -> fromJustDef 0.0 $ M.lookup e lambda) edges))
  master <- run original takeStep 500 $ initialize theta (bbox1, bbox2)
  finalMAP master 
    where
      run original fn n start = foldM doOne start [1..n]
          where doOne a i = trace (show $ m1 ) $ 
                            trace (show $ m2) $ fn a
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