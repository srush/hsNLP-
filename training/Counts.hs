module Counts where 
import TAG
import TAGparse 
import NLP.ChartParse.Eisner
import NLP.Semiring.Derivation
import TreeBank
import Control.Exception
import Control.Monad
import Text.PrettyPrint.HughesPJClass

readTAG f = toTAGDependency `liftM` readSentence f

showCount f = do
  t <- readTAG f
  let c = countTAG t
  putStrLn $ render $ pPrint $ countTAG t

countTAG dsent =
    case semi of 
        Nothing -> throw $ AssertionFailed $ show dsent
        Just s -> fromDerivation s
    where 
      (TAGSentence sent _) = dsent
  --print sent
      getFSM i (Just word) =  (initAdj dsent ALeft word,
                               initAdj dsent ARight word)
      symbolConv word = Just word 
      (semi,chart) =   eisnerParse getFSM symbolConv sent (\ _ i -> i) 
      



-- Query (for making queries about the counts)

