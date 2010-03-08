{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
-- | Beam Search is a pruning technique where low scoring items are filtered out 
--   of a cell. It is not a solver itself, but it can be combined with other solvers
--   to improve the efficiency at the cost of some accuracy.

module Data.DP.Solvers.Beam (
                            -- * Filters
                            BeamFilter,
                            topK, 
                            topWindow, 
                            -- * Solver
                            solveDPBeam) where 

import Data.DP.SolverInternal
import Data.DP.Internals
import Data.Semiring
import Data.Function (on)
import Data.List (sortBy, maximumBy)
import Data.DP


type BeamFilter cell = cell -> cell

-- | @'topK' k cell@ - Keep only the top @k@ items in @cell@.
topK :: (Cell cell, WeightedSemiring (CellVal cell)) => Int -> BeamFilter cell
topK k cell = fromList $ take k $ sortBy (compare `on` snd) $ toList cell 


-- | @'topWindow' delta cell@ - Keep only the items within a @delta@ of the top @cell@.
topWindow :: (Cell cell, WeightedSemiring (CellVal cell)) => CellVal cell -> BeamFilter cell
topWindow delta cell = fromList $ filter ((> cutoff ) . snd ) ls
    where ls = toList cell
          max = maximumBy (compare `on` snd) ls 
          cutoff = (snd max) `mappend` delta

solveDPBeam
  :: (SolveDP s) =>
     (BeamFilter (DCell s)) 
     -> SolveFn s
solveDPBeam beam solver frame dp =  startSolver (return . beam) solver frame dp