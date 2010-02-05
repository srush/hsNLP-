{-# LANGUAGE TypeFamilies #-}
module NLP.ChartParse where
 
--{{{  Imports
import NLP.Semiring
import NLP.WordLattice
import Helpers.Common
import qualified Data.Map as M
import Data.Array
import Text.Printf
import Debug.Trace
--}}}

type Span = (Int, Int) 

-- TODO: It must be possible to O(n) enumerations and O(1) duplicate check.
-- (for now just use Set and eat the extra log) 

-- S bounds the number of items per cell, must be O(1) 
newtype Cell sig semi = Cell (M.Map sig semi)

--{{{  Cell Classes 
uncell (Cell cell) = cell

instance (Show sig, Show semi) => Show (Cell sig semi) where  
    show (Cell m) = intercalate "\n\t" $ map (\(sig,semi) -> show sig ++ "\t" ++ show semi) $  M.toList m


instance (Pretty sig, Pretty semi) => Pretty (Cell sig semi) where  
    pPrint (Cell m) = 
        vcat $                   
        map (\(sig,semi) -> pPrint sig) $                
        M.toList m
--}}}

newtype Chart sig semi = Chart (M.Map Span (Cell sig semi))

chartStats :: Chart sig semi -> String 
chartStats (Chart m) = intercalate "\n" $ do
  (r, Cell cell)<- M.toList m
  return $ (show r) ++ ": " ++ (show $ M.size cell) 

chartLookup :: Span -> Chart sig semi -> Maybe [(sig, semi)] 
chartLookup pos (Chart chart) = do 
  Cell cell <- M.lookup pos chart  
  return $ M.toList $ cell

extractItems :: Chart sig semi -> [(Span, Item sig semi)] 
extractItems (Chart m) = do 
  (span, Cell cell) <- M.toList m  
  (sig, semi) <- M.toList cell
  return (span, (sig, semi))
         
--{{{  Chart Classes
instance (Show sig, Show semi) => Show (Chart sig semi)where  
    show (Chart m) =
        intercalate "\n" $
        map (\((i,j),v) -> printf "[%d:%d] \n\t" i j ++  show v )  $ 
        filter (\(ok, Cell v) -> not $ M.null v) $ M.toList m 

instance (Pretty sig, Pretty semi) => Pretty (Chart sig semi) where 
    pPrint (Chart m)= 
        vcat $
        map (\((i,j),v) -> 
             text (printf "[%d:%d]" i j) $+$  nest 4 (pPrint v)) $    
        filter (\(ok, Cell v) -> not $ M.null v) $ M.toList m
--}}}

type Item sig semi = (sig, semi)

-- A basic mono-lingual chart parser. 
chartParse :: (Semiring semi, Ord sig, WordLattice sent) => 
              sent ->
              (Span -> (Span -> [Item sig semi]) -> ([Item sig semi] -> [Item sig semi]) -> [Item sig semi]) -> 
              (Span -> M.Map sig semi -> M.Map sig semi) -> 
              ([Item sig semi] -> [Item sig semi]) -> 
              Chart sig semi 
chartParse sent combine prune beam  = Chart chart 
    where 
      n = latticeLength sent
      chart = M.fromList $
              [((i,k), Cell $ prune (i,k) $ M.fromListWith mappend $ 
                combine (i,k) (\i-> M.toList $ uncell $ 
                               fromJustNote "lookup fail" $ M.lookup i chart) beam)
                   | d <- [1..n], 
                     i <- [1..n],
                     let k = i + d,
                     k <= n+1]

outsideParse sent (Chart inside) combine  = (Chart chart, mconcat extras) 
    where 
      n = latticeLength sent
      insideL i = M.toList $ uncell $ fromMaybe (Cell M.empty) $ M.lookup i inside 
      chart = M.fromList $ chartdata
      (chartdata, extras) = unzip $ do 
        d <- [n,n-1..1] 
        i <- [1..n]
        let k = i + d
        guard $ k <= n +1
        let (cell, extra) = combine (i,k)  insideL (\i-> M.toList $ uncell $ fromJustNote "lookup fail" $ M.lookup i chart)
        return (((i,k), Cell $ M.fromListWith mappend $ cell), extra) 
              


alignCharts :: (WordLattice sent, Semiring semi, Ord sig) =>
               sent -> 
               Chart sig semi ->
               Chart sig semi ->
               [(sig, (semi,semi))]
alignCharts sent (Chart inside) (Chart outside) =
    concatMap (M.toList . (uncurry $ M.intersectionWithKey (\k a b-> (a, b)))) pairs
    where
      pairs =
              [( uncell $ fromJustNote "lookup fail" $ M.lookup (i,k) inside,
                 uncell $ fromJustNote "lookup fail" $ M.lookup (i,k) outside)
                   | d <- [1..n], 
                     i <- [1..n],
                     let k = i + d,
                     k <= n+1
              ] 
      n = latticeLength sent    

type InitialDerivationRule item = [item]
type SingleDerivationRule item = item -> [item] 
type DoubleDerivationRule item = item -> item ->  [item]
   