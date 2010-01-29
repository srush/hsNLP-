import Helpers.Common
import NLP.Model.Decoding
import NLP.TreeBank.TAG 
import NLP.TreeBank.TreeBank
import NLP.Model.TAGparse
import System (getArgs) 
import qualified Data.Map as M
import NLP.ChartParse
import NLP.Model.ChainPrior
import NLP.Semiring
import NLP.Semiring.Prob
import NLP.ChartParse.Eisner.Inside as EI
import NLP.Semiring.ViterbiNBestDerivation
import Debug.Trace
import Text.Printf
import Data.Array
import Control.Parallel.Strategies
import Data.List
import NLP.Model.Distance
import Data.Binary
import NLP.Grammar.TAG
import System.IO
import NLP.Model.Chain
import NLP.Model.Adjunction
import NLP.Language.English
import NLP.Model.CreateableSemi
import NLP.Model.Derivation
import NLP.Model.TAGWrap

separate :: (Eq el) => el -> [el] -> [[el]]
separate el [] = [] 
separate el ls = case elemIndex el ls of
                   Just n -> 
                       (take n ls): (separate el (drop (n+1) ls))
                   Nothing -> []

main = do 
  [adjCountFile, spineCountFile, spineProbFile, testFile, n] <- getArgs
  params <- (readDecodeParams adjCountFile spineCountFile spineProbFile) :: IO (DecodeParams English)
  contents <- readFile testFile
  hSetBuffering stdout NoBuffering
  let sents =  map (parseSentence testFile. unlines) $ 
               separate "" $ lines contents
  
  let parses = zip [1..] $ map (\s -> let Just b = decodeGold params s in (decodeSentence params s, b) ) sents
  let results = [ ((b', b), n)
                      | (n, (Just b', b)) <- parses]
  
  
  -- putStrLn $ chartStats $ snd $ head results

  mapM_ (\((a,b),_) ->  renderSentences a b) results


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
          
      
