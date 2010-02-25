import System
import NLP.Decomp.Results
import HSH
import NLP.Grammar.Spine
import NLP.Grammar.Dependency
import NLP.Grammar.TAG
import qualified NLP.Grammar.Dependency as D
import qualified Data.Set as S
import qualified Data.Map as M
import NLP.Model.TAG.Semi
import Text.PrettyPrint.HughesPJClass
import Text.Printf
import NLP.Model.TAG.Decoding
import NLP.ParseMonad
import Data.Monoid

data Analysis = Analysis {
      sentN :: Int,
      sentLength :: Int,
      ddFscore :: Double,
      pureFscore :: Double,
      stats :: [Int]
}


type Filter = Analysis -> Bool



converge a = stats a !! 0 
depRight a= stats a !! 1 
psgRight a= stats a !! 2
finalRight a = stats a !! 3
psgChange a= stats a !! 4
depChange a = stats a !! 5

initSpines a = stats a !! 6
finalSpines a = stats a !! 7
changeSpines a = stats a !! 8

betterFScore a = ddFscore a > pureFscore a 
worseFScore a  = ddFscore a < pureFscore a 
noAgreement a  = not (psgChange a == 0 &&  
                      depChange a == 0)
compromise a = min (depRight a) (psgRight a) > (finalRight a)
disagree a  = min (depRight a) (psgRight a) < (finalRight a)

instance Monoid Analysis where
    mempty = Analysis 0 0 0.0 0.0 $ replicate 10 0
    mappend (Analysis _ _ a b c) (Analysis  _ _ a' b' c') =
        Analysis 0 0 (a + a') (b + b') $ zipWith (+) c c'

main = do 
  [resultFile, filtStr] <- getArgs
  let filt = case filtStr of 
               "b" -> betterFScore 
               "w" -> worseFScore
               "i" -> const True
               "n" -> noAgreement
               "d" -> disagree
               "c" -> compromise

               

  contents <- readFile resultFile
  let results = getResults contents
  dm <- loadDebugMappers
  scores' <- mapM (analyze dm) results
  let scores = filter filt scores'  
  let comb = mconcat scores
  putStrLn $ showComb  (map ((/ (fromIntegral $ length scores)) . fromIntegral) $ stats comb) 
  mapM_ (putStrLn. showAnalysis) (scores)
       where 
         showComb ::  [Double] -> String
         showComb a = printf "        %6.2f %6.2f %s"  (0.0::Double) (0.0::Double) $ show $ hcat $ punctuate (text " ") $ map (text .printf "%2.2f") a
                        
         showAnalysis (Analysis sn sl a b c ) = printf "%4d %2d %6.2f %6.2f %s" sn sl a b $ show $ hcat $ punctuate (text " ") $ map (text .printf "%4d") c
         analyze dm result = do
                 fscore <- getFScore (goldStr result) $  testStr result
                 let niceinit = runParseMonad (toNiceSent $ initialTAG result) dm
                 initFScore <- getFScore (goldStr result) niceinit  
                 let conv = roundConverged result
                 let goldPSG = S.fromList $ fixDep $ D.toList $ unTAG $ goldTAG result
                 let initDep = S.fromList $ initialDependency result
                 let initPSG = S.fromList $ fixDep $ D.toList $ unTAG $ initialTAG result
                 let finalDep = S.fromList $ finalDependency result
                 let finalPSG = S.fromList $ fixDep $ D.toList $ unTAG $ finalTAG result

                 let initSpines = getOrderedSpines $ initialTAG result
                 let goldSpines = getOrderedSpines $ goldTAG result
                 let finalSpines = getOrderedSpines $ finalTAG result
                 let n = maximum $ map snd $ finalDependency result
                 return $  Analysis (sentNum result) n fscore initFScore [(maybe 40 id conv) + 1, 
                                                       (diffCount goldPSG initDep), 
                                                       (diffCount goldPSG initPSG), 
                                                       (diffCount goldPSG finalPSG), 
                                                       (diffCount finalPSG initPSG), 
                                                       (diffCount finalDep initDep),
                                                                          (comp initSpines goldSpines),
                                                                          (comp finalSpines goldSpines),
                                                                          (comp initSpines finalSpines)
                                                                         ]

         diffCount a b = S.size $ S.difference a b
         setDiff s1 s2 = (S.size s1, S.size $ S.intersection s1 s2) 
         unTAG (TAGDerivation t) = t 
         getOrderedSpines (TAGDerivation (Dependency m)) = map (dcWord . adjInfo . info . snd) $ M.toList m 
         comp als bls = sum $ zipWith (\a b -> if a == b then 0 else 1) als bls
         fixDep dep = map (\(a,b) -> if b == m then (a,0) else (a,b)) dep 
                 where m = maximum $ map snd dep 