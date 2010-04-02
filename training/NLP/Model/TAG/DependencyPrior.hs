module NLP.Model.TAG.DependencyPrior where 

import Helpers.Common hiding (float)
import NLP.ParseMonad
import NLP.Language.SimpleLanguage
import Helpers.Parse
import Data.Array
import qualified Data.Set as S
import qualified Data.Map as M
import NLP.Model.ParseState 
import NLP.Model.TAG.Adjunction
import NLP.Grammar.Spine
import Control.Exception hiding (try)
import NLP.Model.TAG.Wrap
import NLP.Grammar.TAG
import Debug.Trace
data WordTripSent = WordTripSent {
      words :: Array Int WordTrip
    }
                  deriving Show

data WordTrip = WordTrip {
      wt_ind :: Int,
      deps :: [Trip]
    }
              deriving Show 

parseSingle :: Parser WordTrip
parseSingle = do
  sentNum <- nat 
  wordInd <- nat
  trips <- many $ try parseTrip
  return $ WordTrip { wt_ind = (fromIntegral wordInd),
                      deps =  trips}
  
type Trip = (Int, ATriplet, Double)

parseTrip :: Parser Trip
parseTrip = do 
  adjoinInd <- nat
  tripAtom <- nat
  score <- onlyfloat
  return (fromIntegral adjoinInd, 
          Atom $ fromIntegral  tripAtom,
         score)

parseWordTripSent :: Parser WordTripSent
parseWordTripSent = do
  singles <- many parseSingle 
  return $ WordTripSent $ listArray (1,length singles) singles 
    

type TripletPrune = (Int, M.Map (Int,Int, ATriplet) Double) 

type TripletMapper = (ANonTerm -> Bool, APOS -> Bool, STriplet -> Maybe ATriplet)
cacheDependency :: WordTripSent -> TripletPrune
cacheDependency (WordTripSent wts) = 
    (snd(bounds wts)+1, M.fromList $ do 
  wt <- elems wts
  (head, triplet, score) <- deps wt
  return $ ((wt_ind wt, head, triplet), score))

validByDepPrior :: Double -> TripletMapper -> TripletPrune -> Validity Collins
validByDepPrior cutoff (isnpb, isComma, mapper) (root,pruner) event context =
    if parentInd context == root then True 
    else
        case childSpine event of 
          Nothing -> True
          Just spine ->  (maybe False id $ 
                                do
                                  trip <- mapper (parent, head, top spine)
                                  let m = M.lookup (fromJustNote "goodchild" $ childInd event, 
                                                    parentInd context,
                                                    trip) pruner
                                  return $ maybe False (> cutoff) m) 

                 where parent = if isnpb $ parentNT context then
                                 fromJustNote "isnpb" $ lookupNonTerm ((spinePos context) +1) (twSpine $ parentTWord context)  
                                else parentNT context 
                       head = if maybe False isnpb tmphead then
                                 lookupNonTerm ((spinePos context) -1) (twSpine $ parentTWord context)  
                              else tmphead
                       tmphead = if prevRegular context then 
                                  Just $ parentNT context
                                 else headNT context
                             

readPruning :: String -> IO [TripletPrune]
readPruning file = do
  contents <- readFile file
  let tripsents = map unlines $ separate "" $ lines contents
  mapM parseOne tripsents
      where parseOne str = 
                case parse parseWordTripSent file str  of 
                  Right s -> return $ cacheDependency s
                  Left error -> throw $ AssertionFailed $ show error 
