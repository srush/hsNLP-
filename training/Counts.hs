module Counts where 
import Prelude hiding (catch)
import TAG
import TAGparse 
import NLP.ChartParse.Eisner
import NLP.Semiring.Derivation
import TreeBank
import Control.Exception
import Control.Monad
import Text.PrettyPrint.HughesPJClass
import Debug.Trace
import Data.Monoid
import Distance
import NLP.Semiring.ViterbiNBestDerivation

readTAG f = toTAGDependency `liftM` readSentence f

showCount f = do
  t <- readTAG f
  let c = countTAG t
  putStrLn $ render $ pPrint $ c


countTAG dsent = case semi of 
        Nothing -> trace ("failed to parse" ++ show dsent) mempty -- throw $ AssertionFailed $ show dsent
        Just s -> case s of 
                    (Derivation (Just m)) -> m
                    (Derivation (Nothing)) -> trace ("no derivation" ++ show dsent) mempty
      
    
    where 
      (TAGSentence sent _) = dsent
      ldiscache = mkDistCacheLeft sent
      rdiscache = mkDistCacheRight sent
 
      getFSM i (Just word) =  (initAdj dsent ldiscache ALeft  word False,
                               initAdj dsent rdiscache ARight word False )
      symbolConv word = Just word 
      (semi,chart) =   eisnerParse getFSM symbolConv sent (\ _ i -> i) id 
     


-- Query (for making queries about the counts)

