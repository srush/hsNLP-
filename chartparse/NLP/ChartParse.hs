{-# LANGUAGE TypeFamilies #-}
module NLP.ChartParse where
 
import NLP.Semiring
import Data.Monoid
import qualified Data.Map as M
import Data.Array
import Data.List (intercalate)
import Text.Printf
import Safe (fromJustNote)
import Text.PrettyPrint.HughesPJ
import Text.PrettyPrint.HughesPJClass
-- a distance from start to finish. Sometimes called a span 
-- but use range here to distinguish Eisner's use of span
type Range = (Int, Int) 


-- TODO: It must be possible to O(n) enumerations and O(1) duplicate check.
-- (for now just use Set and eat the extra log) 

-- S bounds the number of items per cell, must be O(1) 
newtype Cell sig semi = Cell (M.Map sig semi)
uncell (Cell cell) = cell

instance (Show sig, Show semi) => Show (Cell sig semi) where  
    show (Cell m) = intercalate "\n\t" $ map (\(sig,semi) -> show sig ++ "\t" ++ show semi) $  M.toList m


instance (Pretty sig, Pretty semi) => Pretty (Cell sig semi) where  
    pPrint (Cell m) = 
        vcat $                   
        map (\(sig,semi) -> pPrint sig {- $$ nest 4 (pPrint  semi) -} ) $                
        M.toList m

newtype Chart sig semi = Chart (M.Map (Int, Int) (Cell sig semi))
type Item sig semi = (sig, semi)


chartLookup :: Range -> Chart sig semi -> Maybe [(sig, semi)] 
chartLookup pos (Chart chart) = do 
  Cell cell <- M.lookup pos chart  
  return $ M.toList $ cell

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

class SentenceLattice a  where
    type Symbol a 
    type LatticeSemi a
    getWords :: a -> Int -> [(LatticeSemi a, Symbol a)]   
    sentenceLength :: a -> Int


-- A basic mono-lingual chart parser. 
chartParse :: (Semiring semi, Ord sig, SentenceLattice sent, semi ~ LatticeSemi sent) => 
              sent ->
              (Range -> (Range -> [Item sig semi]) -> [Item sig semi]) -> 
              Chart sig semi 
chartParse sent combine = Chart chart 
    where 
      n = sentenceLength sent
      chart = M.fromList $

              [((i,k), Cell $ M.fromListWith mappend $ combine (i,k) (\i -> M.toList $ uncell $ fromJustNote "lookup fail" $ M.lookup i chart))
                   | i <- [1 .. n+1],
                     k <- [i+1 .. n+1]]

type InitialDerivationRule item = [item]
type SingleDerivationRule item = item -> [item] 
type DoubleDerivationRule item = item -> item ->  [item]
   