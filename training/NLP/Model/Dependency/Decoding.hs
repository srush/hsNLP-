{-# LANGUAGE BangPatterns , FlexibleContexts #-}
module NLP.Model.Dependency.Decoding where 
-- Various functions to help with decoding, super hacky right now
-- TODO: clean up constants in this file 
import Helpers.Common
import NLP.Model.CreateableSemi
import NLP.Model.ParseTree
import NLP.Model.Dependency.Derivation
import NLP.Model.Dependency.Wrap
import NLP.Model.Dependency.Semi
import NLP.Model.Dependency.Model
import NLP.Model.Dependency.Format
import NLP.Model.Dependency.Parse
import NLP.Grammar.Dependency
import NLP.Grammar.DependencySent

import NLP.Semiring.ViterbiNBestDerivation
import NLP.Probability.Chain
import qualified Data.Map as M
import NLP.ChartParse.Eisner.Inside as EI
import NLP.Semiring.Prob
import NLP.Model.ParseState
import NLP.Grammar.Dependency
import NLP.Model.Distance
import NLP.TreeBank.TreeBank
import NLP.WordLattice
import Data.Binary
import NLP.Language.SimpleLanguage
import Control.DeepSeq
import System.IO.Unsafe
import Control.Concurrent.MVar
import NLP.ParseMonad
import qualified Data.Traversable as T
import NLP.Model.TAG.Semi

import Debug.Trace
type DecodeParams = (Probs FirstOrderDep)

readDecodeParams :: String -> IO DecodeParams
readDecodeParams adjCountFile = do
  !counts <- decodeFile adjCountFile
  let probs = estimate counts
  return $! probs

data DecodingOpts = DecodingOpts {
      validator :: DSentence -> Validity FirstOrderDep,
      commaPrune :: Bool,
      optsLabels :: [ALabel],
      labeler :: Labeler
    }

defaultDecoding =  DecodingOpts {
                     validator = const allValid,
                     commaPrune = True,
                     optsLabels = undefined,
                     labeler = undefined
                   }

decodeSentence :: DecodeParams ->  WordInfoSent -> 
                  ParseMonad (Maybe (CVD DependencyDerivation))
decodeSentence = genDecodeSentence defaultDecoding
                         

globalThresOne :: (BestScorer m Prob s) => Prob -> [(i,s)] -> [(i,s)] 
globalThresOne beamPrune ps =  
    filter (\(_,p) -> (getBestScore p) >= (best / beamPrune)) $ ps  
        where
          best = case map (getBestScore.snd) ps of
               [] -> 0.0
               ls -> maximum $ ls


simplePrune beamPrune ps =  
    M.filter (\p -> {-score p >= (n/100000) && -} getBestScore p >= (best/beamPrune))  ps  
        where
          ps' = M.elems ps 
          score p = p 
          best = case ps' of
               [] -> 0.0
               ls -> maximum $ map getBestScore ls


genDecodeSentence :: DecodingOpts  ->
                     DecodeParams  ->   WordInfoSent -> 
                     ParseMonad (Maybe (CVD DependencyDerivation))
genDecodeSentence  opts probs insent = do 
  testsent <- toDependency (labeler opts) insent 
  sent <- toDepSentence insent
  fsm <- makeFSM  sent testsent opts mkSemi
  let (b',chart)= eisnerParse fsm id sent (\wher -> simplePrune 1e7) (globalThresOne 1e7) -- (\_ i-> i) id --
      
  return b' -- $  trace (show chart) b'
        where 
          getProb = memoize $ prob probs 
          mkSemi pairs = getProb pairs

memoize :: Ord a => (a -> b) -> (a -> b) 
memoize f =
    unsafePerformIO $ 
    do cacheRef <- newMVar M.empty
       return (\x -> unsafePerformIO (g cacheRef x))
    where
      g cacheRef x = 
          do cache <- readMVar cacheRef
             case M.lookup x cache of
               Just y  -> return y
               Nothing -> do 
                 let y     = f x 
                 let cache' = M.insert x y cache
                 swapMVar cacheRef cache'
                 return y


newValid sent event context = 
    hasDependencyAndLabel (dsDep sent) (parentInd context) (childInd event) (childLabel event)


goldDecoding =  DecodingOpts {
                     validator = newValid,
                     commaPrune = False,
                     labeler = undefined,
                     optsLabels = undefined
                   }

decodeGold :: DecodingOpts -> DecodeParams -> WordInfoSent -> ParseMonad (Maybe (CVD DependencyDerivation))
decodeGold opts probs insent = do
    dsent <- toDependency (labeler opts) insent
    let actualsent = dSentence dsent 
    tagsent <- toDepSentence insent
    fsm <- makeFSM tagsent dsent opts mkSemi 
    let (b',_)= eisnerParse fsm id actualsent (\ _ i -> i) id
    return b'
    where
          mkSemi pairs = prob probs pairs

makeFSM :: Sentence DWord -> DSentence  -> DecodingOpts -> 
           (Pairs FirstOrderDep -> Counter (CVD DependencyDerivation)) -> 
           ParseMonad (Int -> DWord ->
                       (AdjState DWord FirstOrderDep (CVD DependencyDerivation) , 
                        AdjState DWord FirstOrderDep (CVD DependencyDerivation) ))
makeFSM insent dsent opts mkSemi  =  do 
      ldiscache <- mkDistCacheLeft insent
      rdiscache <- mkDistCacheRight insent
      
      leftState <- initState (ParseOpts collins ldiscache (ProbModel mkSemi val))  (optsLabels opts) ALeft
      rightState <- initState (ParseOpts collins rdiscache (ProbModel mkSemi val)) (optsLabels opts) ARight 
      return (\ i word -> (leftState i word, rightState i word))
             where collins = commaPrune opts 
                   val = (validator opts) dsent


renderSentences a b = do
           let der1 = getBestDerivation a
           let der2 = getBestDerivation b
           let (Prob sc1) = getBestScore a
           let (Prob sc2) = getBestScore b
           d1 <- depDerToTree der1 
           d2 <- depDerToTree der2 
           let toReadable = (\a -> case a of 
                                     Left anonterm -> do
                                       nonterm <- fromAtom $ anonterm  
                                       return $ Left nonterm 
                                     Right gword -> do
                                       word <- fromAtom $ getLex gword
                                       pos <- fromAtom $ getPOS gword
                                       return $ Right (word, pos))
                                         
           d1' <- T.mapM toReadable d1 
           d2' <- T.mapM toReadable d2

           let st1 = (render $ niceParseTree $ d1')
           let st2 = (render $ niceParseTree $ d2')

           return $ do 
             --let ldiff  = getDiff (lines st1) (lines st2) 
             putStrLn $ "First " ++ (printf "%.3e" sc1) 
             --putStrLn $ render $ vcat $ map (pPrint . snd) $ M.toList diff1
             putStrLn $ "Second" ++ (printf "%.3e" sc2) 
             --putStrLn $ render $ vcat $ map (pPrint . snd) $ M.toList diff2
             --putStrLn $ show ldiff
             putStrLn $ st1
             putStrLn $ st2
             putStrLn $ show $ getBestDerivation b

             putStrLn $ ("G" ++ " " ++ (show $ d1'))
             putStrLn $ ("T" ++ " " ++ (show $ d2'))
