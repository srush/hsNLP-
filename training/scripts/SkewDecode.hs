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
import LP.DualDecomp

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
  
  let results = [ ((b', b), n)
                      | (n, (Just b', Just b)) <- 
                          zip [1..] $ map (\s -> (genDecodeSentence intercept params s, decodeGold params s) ) sents]

  mapM_ (\((a,b),_) ->  do
           let der1 = getCVDBestDerivation a
           let der2 = getCVDBestDerivation b
           let (Prob sc1) = getCVDBestScore a
           let (Prob sc2) = getCVDBestScore b

           let st1 = (render $ niceParseTree $ tagDerToTree der1)
           let st2 = (render $ niceParseTree $ tagDerToTree der2)
           putStrLn $ "First " ++ (printf "%.3e" sc1) 
           putStrLn $ "Second" ++ (printf "%.3e" sc2) 
           putStrLn $ st1
           putStrLn $ st2
           putStrLn $ show $ getCVDBestDerivation b

           putStrLn $ ("G" ++ " " ++ (show $ tagDerToTree der1))
           putStrLn $ ("T" ++ " " ++ (show $ tagDerToTree der2))) results
    where 
      intercept pair = case decisionInfo pair of 
                         (Just (1,3)) -> 1.0
                         _ -> 0.0

