{-# LANGUAGE BangPatterns  #-}
module NLP.Model.TAG.Decoding where 
-- Various functions to help with decoding, super hacky right now
-- TODO: clean up constants in this file 
import Debug.Trace
import Helpers.Common
import NLP.Model.CreateableSemi
import NLP.Model.Derivation
import NLP.Model.TAG.Wrap
import NLP.Model.TAG.Prior
import NLP.Model.TAG.Semi

import NLP.Semiring.ViterbiNBestDerivation
import NLP.Probability.Chain
import qualified Data.Map as M
import NLP.ChartParse.Eisner.Inside as EI
import NLP.Semiring.Prob
import NLP.Grammar.TAG
import NLP.Model.ParseState
import NLP.Grammar.Dependency
import NLP.Model.TAG.Parse
import NLP.Model.Distance
import NLP.Model.TAG.Format
import NLP.Model.TAG.Adjunction
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
type DecodeParams = (SpineExist, Probs Collins, Probs CollinsPrior)

readDecodeParams :: String -> String -> String -> IO DecodeParams
readDecodeParams adjCountFile spineCountFile spineProbFile = do
  !counts <- decodeFile adjCountFile
  !spineCounts <- decodeFile spineCountFile
  !spineCounts2 <- decodeFile spineProbFile
  let probSpine = estimate spineCounts2
  --print counts
  let probs = estimate counts
  return $! (spineCounts, probs, probSpine)
   


data DecodingOpts = DecodingOpts {
      extraDepScore :: (Pairs (Collins) -> Counter (CVD TAGDerivation)),
      validator :: Validity Collins ,
      beamThres :: Double,
      commaPrune :: Bool
    }

defaultDecoding =  DecodingOpts {
                     extraDepScore = const 0.0,
                     validator = allValid,
                     beamThres = 10000,
                     commaPrune = True
                   }

decodeSentence :: 
                  DecodeParams ->  WordInfoSent -> 
                  ParseMonad (Maybe (CVD TAGDerivation))
decodeSentence = genDecodeSentence defaultDecoding
                         

genDecodeSentence ::
                  DecodingOpts  ->
                  DecodeParams  ->   WordInfoSent -> 
                  ParseMonad (Maybe (CVD TAGDerivation))
genDecodeSentence  opts (spineCounts, probs, probSpine) insent = do 
  testsent <- toTAGTest spineCounts insent 
  sent <- toTAGSentence insent
  fsm <- makeFSM  sent (validator opts) mkSemi $ commaPrune opts
  let (b',_)= eisnerParse fsm Just testsent (\ wher m -> prune getSpineProb wher {-$ globalThres thres wher-} m)                             
                  (globalThresOne (beamThres opts) getSpineProb)

  return b'
        where 
              getProb = memoize (prob probs) 
              mkSemi pairs = getProb pairs + (extraDepScore opts) pairs
              getSpineProb = memoize (prob probSpine) 

genDecodeSentenceNew ::
                  DecodingOpts  ->
                  DecodeParams  ->   WordInfoSent -> 
                  ParseMonad (Maybe (CVD TAGDerivation))
genDecodeSentenceNew  opts (spineCounts, probs, probSpine) insent = do 
  testsent <- toTAGTest spineCounts insent 
  sent <- toTAGSentence insent
  fsm <- makeFSM  sent (validator opts) mkSemi $ commaPrune opts
  let (b',_)= eisnerParse fsm Just testsent (\ wher m -> prune getSpineProb wher {-$ globalThres thres wher-} m)                             
                  (globalThresOne (beamThres opts) getSpineProb)

  return b'
        where 
              getProb = memoize (prob probs) 
              mkSemi pairs = getProb pairs + (extraDepScore opts) pairs
              getSpineProb = memoize (prob probSpine) 


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

newValid sent  event context = 
    valid sent (parentTWord context) (childTWord event) (spinePos context) (fromJustDef Sister $ NLP.Model.TAG.Adjunction.adjType event)


decodeGold :: DecodeParams -> WordInfoSent -> ParseMonad (Maybe (CVD TAGDerivation))
decodeGold (spineCounts, probs, probSpine) insent = do
    dsent <- toTAGDependency insent
    let actualsent = tSentence dsent 
        prunVal = newValid dsent
    tagsent <- toTAGSentence insent
    fsm <- makeFSM tagsent prunVal mkSemi False
    let (b',_)= eisnerParse fsm Just actualsent (\ wher m -> globalThres 0.0 wher m) id
    return b'
    where
          mkSemi pairs = prob probs pairs




makeFSM :: Sentence TWord  -> (Validity Collins) -> 
           (Pairs Collins -> Counter (CVD TAGDerivation)) -> 
           Bool -> 
           ParseMonad (Int -> Maybe TWord ->
                       (AdjState TWord Collins (CVD TAGDerivation) , 
                        AdjState TWord Collins (CVD TAGDerivation) ))
makeFSM insent val mkSemi collins =  do 
      ldiscache <- mkDistCacheLeft insent
      rdiscache <- mkDistCacheRight insent
      
      leftState <- initState (ParseOpts collins ldiscache (ProbModel mkSemi val)) [] ALeft
      rightState <- initState (ParseOpts collins rdiscache (ProbModel mkSemi val)) [] ARight 
      return (\ i (Just word) -> (leftState i word, rightState i word))


globalThres n wher m =
    M.filter (\p -> getBestScore p >= n/100000) $  m    

globalThresOne beamPrune probs  ps =  
    filter (\p -> {-score p >= (n/100000) && -} score p >= (best/beamPrune))  ps  
        where
          score p = getFOM probs p 
          best = case  map (getFOM probs) ps of
               [] -> 0.0
               ls -> maximum $ ls



getFOM probs (sig, semi) = getPrior sig * getInside semi 
    where
      getProb = (\a  -> probs a) 
      getPrior sig = (if hasParent $ leftEnd sig then 1.0 else 
                          (prior (\tword -> getProb $ chainRule (PrEv tword) (PrCon ())) (EI.word $ leftEnd sig) )) * 
                     (if hasParent $ rightEnd sig then 1.0 else 
                          (prior (\tword -> getProb $ chainRule (PrEv tword) (PrCon ())) $ EI.word $ rightEnd sig)) 
      getInside i  =  p
          where (Prob p) = getBestScore i 


prune probs wher m = 
    --(trace ((printf "Best for %s is : %s %s %s  " (show wher ) (show bestNH) (show bestR) (show bestL)) ++ (show (Cell s))) )  
 s 
     where 
      s = M.filterWithKey (\sig semi -> ( -- getFOM (sig,semi)) > (best / 10000) --
                                         if hasNoAdjoin (sig,semi) then
                                             getFOM probs (sig,semi) >= (bestNH / 10000) || isNaN bestNH 
                                         else if hasAdjoinL (sig,semi) then
                                             getFOM probs (sig,semi) >= (bestL / 10000) || isNaN bestL
                                         else if hasAdjoinR (sig,semi) then
                                             getFOM probs (sig,semi) >= (bestR / 10000) || isNaN bestR 
                                        else True)
                                        --    (getFOM (sig,semi)) > (bestH / 50000)
                                        --else  (getFOM (sig,semi)) > (bestNH / 50000)
                          ) m    
      --p' = M.filter (\(_,fom) -> fom >= (bestH / 50000)) $ M.mapWithKey (\sig semi -> (semi, getFOM (sig,semi))) m    
      --p = M.filter (\(_,fom) -> fom <= (bestH / 50000)) $ M.mapWithKey (\sig semi -> (semi, getFOM (sig,semi))) m    

      hasNoAdjoin (sig, semi) = hasParentPair sig == (False, False) 
      hasAdjoinL (sig, semi) = hasParentPair sig == (True, False)
      hasAdjoinR (sig, semi) = hasParentPair sig == (False, True) 
      best = case  map (getFOM probs )$ M.toList m of
               [] -> 0.0
               ls -> maximum $ ls
      bestL = case  map (getFOM probs) $ filter hasAdjoinL $ M.toList m of
               [] -> 0.0
               ls -> maximum $ ls
      bestR = case  map (getFOM probs) $ filter hasAdjoinR $ M.toList m of
               [] -> 0.0
               ls -> maximum $ ls

      bestNH = case  map (getFOM probs) $ filter hasNoAdjoin $ M.toList m of
               [] -> 0.0
               ls -> maximum $ ls




renderSentences a b = do
           let der1 = getBestDerivation a
           let der2 = getBestDerivation b
           let (Prob sc1) = getBestScore a
           let (Prob sc2) = getBestScore b
           d1 <- tagDerToTree der1 
           d2 <- tagDerToTree der2 
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

basicParams :: IO DecodeParams
basicParams = 
    readDecodeParams "/tmp/curcounts" "/tmp/cspines" "/tmp/pspines"
                     
