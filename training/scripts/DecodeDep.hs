import Helpers.Common
import NLP.ParseMonad
import NLP.TreeBank.Label 
import NLP.TreeBank.TreeBank
import System (getArgs) 
import qualified Data.Map as M
import NLP.ChartParse
import NLP.Semiring
import NLP.Semiring.Prob
import NLP.ChartParse.Eisner.Inside as EI
import NLP.Semiring.ViterbiNBestDerivation
import Text.Printf
import Data.Array
import Control.Parallel.Strategies
import Data.List
import NLP.Model.Distance
import Data.Binary
import NLP.Grammar.TAG
import System.IO

import NLP.Model.Dependency.Decoding

separate :: (Eq el) => el -> [el] -> [[el]]
separate el [] = [] 
separate el ls = case elemIndex el ls of
                   Just n -> 
                       (take n ls): (separate el (drop (n+1) ls))
                   Nothing -> []

main = do 
  [adjCountFile, label, testFile, n] <- getArgs
  params <- (readDecodeParams adjCountFile) :: IO (DecodeParams)
  contents <- readFile testFile
  mappers <- loadDebugMappers 

  hSetBuffering stdout NoBuffering
  let sentsM =  mapM (parseSentence testFile. unlines) $ 
               separate "" $ lines contents
  let sents = runParseMonad sentsM mappers  
  let labels = runParseMonad (snd $ pickSelect label) mappers
  let labeler =runParseMonad (fst $ pickSelect label) mappers
  putStrLn "Loaded Params"
  let parses =  mapM (\s -> do 
                        -- Just b <- decodeGold goldDecoding{optsLabels = map convertNTtoLabel labels, labeler=labeler} params s
                        Just b' <- genDecodeSentence defaultDecoding{optsLabels = map convertNTtoLabel labels, labeler=labeler} params s 
                        return (Just b', b') ) sents
  let results = [ (b', b)
                      |  (Just b', b) <- runParseMonad parses mappers]
  
  -- putStrLn $ chartStats $ snd $ head results

  sequence $ runParseMonad (mapM (\(a,b) ->  renderSentences a b) results) mappers 


      --print "The fixed parse answer"
  --putStrLn $ ("G" ++ (show $ tagDerToTree  $ getBestDerivation b'''))
  --putStrLn $ ("T" ++ (show $ tagDerToTree  $ getBestDerivation b))
 -- print "The fixed spine answer"
 -- print $ tagDerToTree $ getBestDerivation  b' 
 -- print "The test answer"
  --print $ tagDerToTree $ getBestDerivation  b 

  --let (TAGDerivation dep)= getBestDerivation b
  --print $ TAGDerivation (dep)
  --let wrong = score d (fmap (\adj -> AdjunctionInfo{TAG.adjPos = TAG.adjPos adj, adjType = adjType adj,adjInfo = ()})  dep)
  --if length wrong == 0 then putStrLn "Perfect."
   --else putStrLn $ render $ vcat $ map (text.show) wrong
          
      
