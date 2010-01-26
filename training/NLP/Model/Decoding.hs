module NLP.Model.Decoding where 
-- Various functions to help with decoding, super hacky right now
-- TODO: clean up constants in this file 

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

readDecodeParams :: (Language l) =>  String -> String -> String -> IO (DecodeParams l)
readDecodeParams adjCountFile spineCountFile spineProbFile = do
  counts <- decodeFile adjCountFile
  spineCounts <- decodeFile spineCountFile
  spineCounts2 <- decodeFile spineProbFile
  let probSpine = estimate spineCounts2
  --print counts
  let probs = estimate counts
  return (spineCounts, probs, probSpine)
   


type DecodeParams l = (SpineExist l, Probs (Collins l), Probs (CollinsPrior l))

decodeSentence :: (Language l) =>
                  DecodeParams l -> Prob -> WordInfoSent l -> 
                  Maybe (CVD l)
decodeSentence = genDecodeSentence (const 0.0)
                         

genDecodeSentence :: (Language l) =>
                  (Pairs (Collins l) -> Counter CVD) -> 
                 
                  DecodeParams l ->  Prob ->  WordInfoSent l -> 
                  Maybe (CVD l)
genDecodeSentence intercept (spineCounts, probs, probSpine) thres  insent = b'
    where (b',_)= eisnerParse fsm Just sent (\ wher m -> prune probSpine wher $ globalThres thres wher m) id
          sent = toTAGTest spineCounts insent 
          mkSemi pairs = prob probs pairs + intercept pairs
          fsm = makeFSM (toTAGSentence insent) allValid mkSemi True
                                                           
          --intercept (Pairs )

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

globalThresOne probs (Prob n) ps =  
    filter (\p -> score p >= (n/100000) && score p >= (best/10000))  ps  
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

basicParams :: IO (DecodeParams English)
basicParams = 
    readDecodeParams "/tmp/curcounts" "/tmp/cspines" "/tmp/pspines"
                     
