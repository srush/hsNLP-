module Counts where 
import Prelude hiding (catch)
import NLP.Grammar.TAG
import NLP.Model.TAGparse 
import NLP.ChartParse.Eisner.Inside
import NLP.ChartParse.Eisner.Outside
import NLP.Semiring.Derivation
import NLP.TreeBank.TreeBank
import Control.Exception
import NLP.Model.Distance
import NLP.Semiring.ViterbiNBestDerivation
import Debug.Trace
import Helpers.Common
import NLP.TreeBank.TAG
import NLP.Model.CreateableSemi
import NLP.Model.Adjunction
import NLP.Language.SimpleLanguage
import NLP.Model.Chain
import NLP.Model.TAGWrap
import NLP.WordLattice
import NLP.ParseMonad
    
readTAG :: String -> IO (ParseMonad TSentence) 
readTAG f = do 
  sent <- readSentence f
  return $ sent >>= toTAGDependency  

readSent :: String -> IO (ParseMonad WordInfoSent)
readSent f =  readSentence f

showCount f = do
   t <- readTAG f
   let c = t >>= countTAG
   return c
   --putStrLn $ render $ pPrint $ fst $ c


-- showOutside f = do
--   t <- readTAG f
--   let (c,_) = countTAG t
--   let o = countOutside t c
--   putStrLn $ render $ pPrint o
               
-- countOutside dsent inside =
--     eisnerOutside sent inside
--     where (inchart, _) = countTAG dsent
--           (TAGSentence sent _) = dsent

type TAGCountState = AdjState Collins TAGCountSemi
type TAGCountSemi = CD (Observation Collins)
 

type BasicOpts = ParseOpts Collins TAGCountSemi  

makeParseOpts ::  TSentence -> ParseMonad (BasicOpts, BasicOpts)
makeParseOpts dsent = do 
  left <- mkDistCacheLeft sent
  right <- mkDistCacheRight sent
  return $ 
    (opts{ distanceCache = left } , 
     opts{ distanceCache = right})
          where 
            sent = tSentence dsent
            opts = ParseOpts {useCommaPruning = False,
                              distanceCache = undefined,
                              model = ProbModel {
                                        validity = valid dsent,
                                        probs = (\p -> observe p)
                                      }
                             }
              
--countTAG :: TSentence  -> ParseMonad TAGDerivation 
countTAG dsent  = do 
        (lopts, ropts) <- makeParseOpts dsent 
        lstate <- initState lopts ALeft
        rstate <- initState ropts ARight
        let getFSM = (\ _ (Just word) -> (lstate word, rstate word))
            (semi,chart) =   eisnerParse getFSM symbolConv sent (\ _ i -> i) id   
        return $ case semi of 
          Nothing -> trace ("failed to parse" ) (mempty,undefined) -- throw $ AssertionFailed $ show dsent
          Just s -> case s of 
                    (CD (Derivation (Just m))) ->  (m,chart)
                    (CD (Derivation (Nothing))) -> trace ("no derivation") (mempty,undefined)
    where 
      sent = tSentence dsent
      symbolConv word = Just word 

     


-- Query (for making queries about the counts)

