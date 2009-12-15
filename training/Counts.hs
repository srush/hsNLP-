module Counts where 
import Prelude hiding (catch)
import NLP.Grammar.TAG
import NLP.Model.TAGparse 
import NLP.ChartParse.Eisner
import NLP.ChartParse.Outside
import NLP.Semiring.Derivation
import NLP.TreeBank.TreeBank
import Control.Exception
import NLP.Model.Distance
import NLP.Semiring.ViterbiNBestDerivation
import Debug.Trace
import Helpers.Common
import NLP.TreeBank.TAG
import NLP.Language
import NLP.Model.CreateableSemi
import NLP.Model.Adjunction
import NLP.Language.English
import NLP.Model.Chain

readTAG :: String -> IO (TAGSentence English)
readTAG f = toTAGDependency `liftM` readSentence f


readSent :: String -> IO (WordInfoSent English)
readSent f =  readSentence f

showCount f = do
   t <- readTAG f
   putStrLn $ show t 
   let c = countTAG (t::TAGSentence English)
   putStrLn $ show $ c


-- showOutside f = do
--   t <- readTAG f
--   let (c,_) = countTAG t
--   let o = countOutside t c
--   putStrLn $ render $ pPrint o
               
-- countOutside dsent inside =
--     eisnerOutside sent inside
--     where (inchart, _) = countTAG dsent
--           (TAGSentence sent _) = dsent

type TAGCountState l = AdjState (Collins l) (TAGCountSemi l) l
type TAGCountSemi l = CD (Observation (Collins l))

type BasicOpts l = ParseOpts (Collins l) (TAGCountSemi l) l

makeParseOpts :: (Language l) => TAGSentence l -> (BasicOpts l, BasicOpts l)
makeParseOpts dsent = 
    (opts{ distanceCache = mkDistCacheLeft sent} , 
     opts{ distanceCache = mkDistCacheRight sent})
          where 
            (TAGSentence sent _) = dsent
            opts = ParseOpts {useCommaPruning = False,
                              distanceCache = undefined,
                              model = ProbModel {
                                        validity = valid dsent,
                                        probs = (\p -> observe p)
                                      }
                             }
              
--countTAG :: (Language l) => TAGSentence l -> TAGDerivation l 
countTAG dsent  = case semi of 
        Nothing -> trace ("failed to parse" ++ show chart) undefined -- throw $ AssertionFailed $ show dsent
        Just s -> case s of 
                    (CD (Derivation (Just m))) ->  (m,chart)
                    (CD (Derivation (Nothing))) -> trace ("no derivation") undefined
          
    where 
      (TAGSentence sent _) = dsent
      (lopts, ropts) = makeParseOpts dsent 
      getFSM i (Just word) =  (initState lopts ALeft  word,
                               initState ropts ARight word )
      symbolConv word = Just word 
      (semi,chart) =   eisnerParse getFSM symbolConv sent (\ _ i -> i) id 
     


-- Query (for making queries about the counts)

