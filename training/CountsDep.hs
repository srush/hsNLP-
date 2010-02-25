{-# LANGUAGE ScopedTypeVariables #-}
module CountsDep where 
import Prelude hiding (catch)
import NLP.Grammar.TAG
import NLP.Model.Dependency.Parse 
import NLP.ChartParse.Eisner.Inside
import NLP.ChartParse.Eisner.Outside
import NLP.Semiring.Derivation
import NLP.TreeBank.TreeBank
import Control.Exception
import NLP.Model.Distance
import NLP.Semiring.ViterbiNBestDerivation
import Debug.Trace
import Helpers.Common
import NLP.Model.Dependency.Format
import NLP.Model.CreateableSemi
import NLP.Model.Dependency.Model
import NLP.Language.SimpleLanguage
import NLP.Probability.Chain
import NLP.WordLattice
import NLP.ParseMonad
import NLP.Model.ParseState
import NLP.Grammar.Dependency    
import NLP.Grammar.DependencySent
import NLP.Model.Dependency.Wrap
import NLP.Model.Dependency.Parse

import NLP.TreeBank.Label

test labeler file = do
  sent <- readSentence file
  return $ do 
    s <- sent
    ctl <- labeler
    t <- toDependency ctl s 
    nts <- enumerate >>= (mapM toAtom)
    fst `liftM` countDep (map convertNTtoLabel nts) t
  
type DepCountState = AdjState DWord FirstOrderDep DepCountSemi
type DepCountSemi = CD (Observation FirstOrderDep) FirstOrderDep
 
type BasicOpts = ParseOpts FirstOrderDep DepCountSemi  

makeParseOpts ::  DSentence -> ParseMonad (BasicOpts, BasicOpts)
makeParseOpts dsent = do 
  left <- mkDistCacheLeft sent
  right <- mkDistCacheRight sent
  return $ 
    (opts{ distanceCache = left } , 
     opts{ distanceCache = right})
          where 
            sent = dSentence dsent
            opts = ParseOpts {useCommaPruning = False,
                              distanceCache = undefined,
                              model = ProbModel {
                                        validity = newValid dsent,
                                        probs = (\p -> observe p)
                                      }
                             }
              
newValid sent event context = 
    hasDependencyAndLabel (dsDep sent) (parentInd context) (childInd event) (childLabel event)
                          

--countTAG :: TSentence  -> ParseMonad TAGDerivation 
countDep labels dsent  = do 
        (lopts, ropts) <- makeParseOpts dsent 
        lstate <- initState lopts labels ALeft
        rstate <- initState ropts labels ARight
        let getFSM = (\ (i::Int) (word::DWord) -> (lstate i word, rstate i word))
            (semi,chart) =   eisnerParse getFSM id sent (\ _ i -> i) id   
        return $ case semi of 
          Nothing -> trace ("failed to parse" ) (mempty,undefined) -- throw $ AssertionFailed $ show dsent
          Just s -> case s of 
                    (CD (Derivation (Just m))) ->  (m,chart)
                    (CD (Derivation (Nothing))) -> trace ("no derivation") (mempty,undefined)
    where 
      sent = dSentence dsent


    
-- Query (for making queries about the counts)

