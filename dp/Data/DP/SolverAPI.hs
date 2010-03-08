{-# LANGUAGE TypeFamilies #-}
module Data.DP.SolverAPI (
                          -- * Solvers 
                          solveDP, solveSimpleDP, 
                          -- * Solutions
                          DPSolution (..), getSimpleResult, 
                          -- * Custom Solver Creation
                          mkSolver) where 

import Data.DP
import Data.DP.Internals as I
import Data.DP.SolverInternal
import Control.Monad.Identity

getSimpleResult :: DPSolution chart ind (Identity r) z -> r  
getSimpleResult = runIdentity . getResult 

-- | Apply a solver created with @'mkSolver'@ to a @'DP'@ to compute a @'DPSolution'@. 
--   @'solveDP' solver frame dp @ will use @solver@ to compute 
--   the result of @dp@. @frame@ is a value defined by solver, usually the final index for top down style solvers
--   and the full ordering for bottom-up solvers.     
solveDP :: (SolveDP s) => SolveFn s
solveDP solver dp frame = startSolver return solver dp frame  


-- | Helper function. Identical to @'solveDP'@ but for @'SimpleDP'@. 
solveSimpleDP :: (Identity b ~ DCell s, SolveDP s) =>
     s
     -> Frame s
     -> SimpleDP (Ind s) b
     -> DPMonad s (DPSolution (Chart s) (Ind s) (DCell s) (Internal s))
solveSimpleDP a b c = solveDP a b (fromSimple c)

-- | Create a custom solver from one of the interfaces in @'Data.DP.Solvers'@
mkSolver :: strategy monad chart ind cell internal
            -> DPSolver monad strategy chart ind cell internal
mkSolver = DPSolver
