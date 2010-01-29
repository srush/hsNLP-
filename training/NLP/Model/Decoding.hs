{-# LANGUAGE BangPatterns  #-}
module NLP.Model.Decoding where 
-- Various functions to help with decoding, super hacky right now
-- TODO: clean up constants in this file 
import Debug.Trace
import Helpers.Common
import NLP.Model.CreateableSemi
import NLP.Model.Derivation
import NLP.Model.TAGWrap
import NLP.Model.ChainPrior
import NLP.Model.Chain
import qualified Data.Map as M
import NLP.ChartParse.Eisner.Inside as EI
import NLP.Semiring.Prob
import NLP.Grammar.TAG
import NLP.Model.TAGparse
import NLP.Model.Distance
import NLP.TreeBank.TAG
import NLP.Model.Adjunction
import NLP.TreeBank.TreeBank
import NLP.Language
import NLP.WordLattice
import Data.Binary
import NLP.Language.English
import Control.DeepSeq
import System.IO.Unsafe
import Control.Concurrent.MVar

type DecodeParams l = (SpineExist l, Probs (Collins l), Probs (CollinsPrior l))
readDecodeParams :: (Language l) =>  String -> String -> String -> IO (DecodeParams l)
readDecodeParams adjCountFile spineCountFile spineProbFile = do
  !counts <- decodeFile adjCountFile
  !spineCounts <- decodeFile spineCountFile
  !spineCounts2 <- decodeFile spineProbFile
  let probSpine = estimate spineCounts2
  --print counts
  let probs = counts `deepseq` estimate counts
  return $! (spineCounts, probs, probSpine)
   


data DecodingOpts l = DecodingOpts {
      extraDepScore :: (Pairs (Collins l) -> Counter CVD),
      validator :: Validity l,
      beamThres :: Double,
      commaPrune :: Bool
    }

defaultDecoding =  DecodingOpts {
                     extraDepScore = const 0.0,
                     validator = allValid,
                     beamThres = 10000,
                     commaPrune = True
                   }

decodeSentence :: (Language l) =>
                  DecodeParams l ->  WordInfoSent l -> 
                  (Maybe (CVD l))
decodeSentence = genDecodeSentence defaultDecoding
                         

genDecodeSentence :: (Language l) =>
                  DecodingOpts l ->
                  DecodeParams l ->   WordInfoSent l -> 
                  Maybe (CVD l)
genDecodeSentence  opts (spineCounts, probs, probSpine)  insent =
    b'
        where (b',_)= eisnerParse fsm Just sent (\ wher m -> prune probSpine wher {-$ globalThres thres wher-} m)                             
                  (globalThresOne (beamThres opts) probSpine)

              sent = toTAGTest spineCounts insent 
              getProb = memoize (prob probs) 
              mkSemi pairs = getProb pairs + (extraDepScore opts) pairs
              fsm = makeFSM (toTAGSentence insent) (validator opts) mkSemi $ commaPrune opts


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


decodeGold :: (Language l) => 
                  DecodeParams l -> WordInfoSent l -> 
                  Maybe (CVD l)
decodeGold (spineCounts, probs, probSpine) insent = b'
    where (b',_)= eisnerParse fsm Just actualsent (\ wher m -> globalThres 0.0 wher m) id
          actualsent = tSentence dsent 
          mkSemi pairs = prob probs pairs
          fsm = makeFSM (toTAGSentence insent) prunVal mkSemi False
          prunVal = valid dsent
          dsent = toTAGDependency insent


makeFSM :: (Language l) => Sentence (TWord l)  -> Validity l -> 
           (Pairs (Collins l) -> Counter CVD) -> 
           Bool -> Int ->
           Maybe (TWord l) ->
           (AdjState (Collins l) CVD  l, 
            AdjState (Collins l) CVD  l)
makeFSM insent val mkSemi collins i (Just word) =  
    (initState (ParseOpts collins ldiscache (ProbModel mkSemi val)) ALeft word,
     initState (ParseOpts collins rdiscache (ProbModel mkSemi val)) ARight word )

    where
      ldiscache = mkDistCacheLeft insent
      rdiscache = mkDistCacheRight insent

globalThres n wher m =
    M.filter (\p -> getCVDBestScore p >= n/100000) $  m    

globalThresOne beamPrune probs  ps =  
    filter (\p -> {-score p >= (n/100000) && -} score p >= (best/beamPrune))  ps  
        where
          score p = getFOM probs p 
          best = case  map (getFOM probs) ps of
               [] -> 0.0
               ls -> maximum $ ls



getFOM probs (sig, semi) = getPrior sig * getInside semi 
    where
      getProb = (\a  -> prob probs $  PrPair a ) 
      getPrior sig = (if hasParent $ leftEnd sig then 1.0 else (prior getProb $ EI.word $ leftEnd sig)) * 
               (if hasParent $ rightEnd sig then 1.0 else (prior getProb $ EI.word $ rightEnd sig)) 
      getInside i  =  p
          where (Prob p) = getCVDBestScore i 


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
           let der1 = getCVDBestDerivation a
           let der2 = getCVDBestDerivation b
           let (Prob sc1) = getCVDBestScore a
           let (Prob sc2) = getCVDBestScore b
           --let TAGDerivation (_, debug1) = der1  
           --let TAGDerivation (_, debug2) = der2
           --let m1 = M.fromList debug1
           --let m2 = M.fromList debug2
           --let diff1 = M.difference m1 m2 
           --let diff2 = M.difference m2 m1
           let st1 = (render $ niceParseTree $ tagDerToTree der1)
           let st2 = (render $ niceParseTree $ tagDerToTree der2)
           --let ldiff  = getDiff (lines st1) (lines st2) 
           putStrLn $ "First " ++ (printf "%.3e" sc1) 
           --putStrLn $ render $ vcat $ map (pPrint . snd) $ M.toList diff1
           putStrLn $ "Second" ++ (printf "%.3e" sc2) 
           --putStrLn $ render $ vcat $ map (pPrint . snd) $ M.toList diff2
           --putStrLn $ show ldiff
           putStrLn $ st1
           putStrLn $ st2
           putStrLn $ show $ getCVDBestDerivation b

           putStrLn $ ("G" ++ " " ++ (show $ tagDerToTree der1))
           putStrLn $ ("T" ++ " " ++ (show $ tagDerToTree der2))

basicParams :: IO (DecodeParams English)
basicParams = 
    readDecodeParams "/tmp/curcounts" "/tmp/cspines" "/tmp/pspines"
                     
