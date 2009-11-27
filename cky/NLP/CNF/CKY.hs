{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-}
module NLP.CNF.CKY where 

--{{{  Imports
import Helpers.Common hiding (Derivation)
import NLP.CNF
import NLP.Semiring.Viterbi
import NLP.Semiring.Derivation
import NLP.Semiring.Prob
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

import LP.DualDecomp
import NLP.Probability.Observation
import NLP.Probability.Distribution hiding (Prob)

--}}}


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
                   P.Grammar NT (ViterbiDerivation (S.Set EdgeVar)) sent  
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
                    mkViterbi $  Weighted (Prob $ theta edgevar  , 
                                           mkDerivation $ S.singleton edgevar) 
                (TerminalRule a p, Nothing) -> 
                    let edgevar = TermVar a p i in 
                    mkViterbi $  Weighted (Prob $ theta edgevar , 
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
     filter (\(_,p) -> (getWeight $ fromViterbi p) >= Prob 1e-110 &&  (getWeight $ fromViterbi p) >= (best/10000)) ps  
         where
           best = case map (getWeight . fromViterbi . snd) ps of
                [] -> 0.0
                ls -> maximum $ ls

-- doParse :: [POS] -> 
doParse sent grammar nts bnts = P.ckyParse sent grammar prune
--doLPParse sent grammar nts bnts = P.ckyParse sent (toLPGrammar grammar nts bnts) pruneLP

doViterbiParse sent theta nts bnts correct = P.ckyParse sent (toViterbiGrammar theta nts bnts correct) pruneV 

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


testViterbi = do 
  [(sent, spanMap)] <- readSentenceBunch "onetree"
  p <- decodeFile "model/outmodel"
  bnts <- decodeFile "ntpairs"
  bpos <- decodeFile "ntpos"
  --nts <- readNT "nonterm"
  let gram = Grammar $ estimate p
  let (Just a, chart) = doViterbiParse (mkSentence $ map mkPOS sent) (mkTheta gram) (\p -> maybe [] S.toList $ M.lookup p bpos) bnts Nothing -- (Just (\sp -> M.lookup sp spanMap))
  return $ a

testLPParse = do 
  [(sent,_)] <- readSentenceBunch "onetree"
  p <- decodeFile "model/outmodel"
  bnts <- decodeFile "ntpairs"
  bpos <- decodeFile "ntpos"
  nts <- readNT "nonterm"
  let gram = Grammar $ estimate p
  let g2 = toInsideGrammar gram (\l -> nts) bnts
  let (Just a, chart) = doParse (mkSentence $ map mkPOS sent) g2 (\l -> nts) bnts 
  

  let (outchart, extras) = trace (show a) $ O.ckyOutside (mkSentence $ map mkPOS sent) chart g2 (\semi -> (semi/ a) > 1e-5) 
  
  let lexedges = trace (show (S.toList $ S.fromList $ map toEdgeVar $ concat extras)   ++ show extras) $ do 
                (i, pos) <- zip [1..] $ map mkPOS sent
                nt <- S.toList $ fromJustDef mempty $ M.lookup pos bpos
                return $ TermVar nt pos i 
  return $ writeCplex $ mkLP $ edgeSetToInfo gram (length sent) $ (S.toList $ S.fromList $ map toEdgeVar $ concat extras) 
  --return chart 
-- testLPparse = do 
--   [sent] <- readSentenceBunch "onetree"
--   return $ writeCplex $ mkLP $ fromDerivation $ doLPParse 5 sent

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
    trace (show outchart) $ (S.toList $ S.fromList $ map toEdgeVar $ concat extras, gram) 
    where 
      g2 = toInsideGrammar gram (\l -> nts) bnts
      (Just a, chart) = doParse (mkSentence $ map mkPOS sent) g2 (\l -> nts) bnts 
      (outchart, extras) = O.ckyOutside (mkSentence $ map mkPOS sent) chart g2 (\semi -> True )--(semi/ a) > 1e-5)   
      

constructGraph = do 
  [(sent,_)] <- readSentenceBunch "onetree"  
  (ntobs, adjobs, parobs) <- readGraphParams
  ip <- readInsideParams
  let (edges,g2) = getEdges sent ip
  let infAdj = trace (show edges) $ informationAdj (finish ntobs,finish adjobs) 
  --return $ mkGraphFromEdges infAdj g2 edges
  -- putStr $ render $ writeGraph $ mkGraphFromEdges infAdj g2 edges
  solveGraph $ mkGraphFromEdges infAdj g2 edges

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