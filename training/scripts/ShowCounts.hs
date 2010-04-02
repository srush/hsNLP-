import Helpers.Common
import Data.List 
import NLP.Model.TAG.Decoding
import NLP.Language.SimpleLanguage
import System.IO
import System
import NLP.TreeBank.TreeBank
import NLP.ParseMonad
import NLP.Model.TAG.DependencyPrior
import NLP.Model.CreateableSemi
import NLP.Model.TAG.Semi
import NLP.ParseMonad
import Debug.Trace
import NLP.Model.TAG.Adjunction
import NLP.Probability.Chain
main = do 
  [adjCountFile, spineCountFile, spineProbFile, pruneFile, testFile, n] <- getArgs
  params <- (readDecodeParams adjCountFile spineCountFile spineProbFile) :: IO (DecodeParams)
  contents <- readFile testFile
  mappers <- loadDebugMappers 
  hSetBuffering stdout NoBuffering
  let sentsM =  mapM (parseSentence testFile. unlines) $ 
               separate "" $ lines contents
  let sents = runParseMonad sentsM mappers  

  prunes <- readPruning pruneFile
  let parses =  mapM (\(s,p) -> do
                        tm <- tripletMapper
                        Just b <- (genDecodeGold (defaultGoldDecoding {validator = (\s e c-> if newValid s e c then 
                                                                                                if not $ validByDepPrior 1e-8 tm p e c then 
                                                                                                    trace ("Pruning fail " ++ (render $ runParseMonad (dumpPairs $  chainRule e c) mappers)) True
                                                                                                else
                                                                                                    True
                                                                                            else False
                                                                                  ) , fixedPOS = False} ) params s) :: ParseMonad (Maybe (CVD TAGDerivation)) 
                        b' <- (genDecodeSentence (defaultDecoding {-validator = newValid-} {listPruning = False, beamThres = 1e5, validator=const $ validByDepPrior 1e-8 tm p, fixedPOS = False} ) params s) :: ParseMonad (Maybe (CVD TAGDerivation)) 
                        b'' <- case b' of 
                                 Just b -> return $ Just  b
                                 Nothing -> trace "failed" $ genDecodeSentence (defaultDecoding {listPruning = False, beamThres = 1e5 } ) params s 
                        return (b'', b)) $ zip sents prunes
  let results = [(b', b)
                      |  (Just b', b) <- runParseMonad parses mappers]
  
  
  -- putStrLn $ chartStats $ snd $ head results

  sequence $ runParseMonad (mapM (\(b,a) -> renderSentences a b) results) mappers 
      where isNothing Nothing = True
            isNothing _ = False

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
          
      
