{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, TypeFamilies, KindSignatures #-}


{-|

Top-Down Recursive Solver. Solve the DP brute-force, no-frills by doing it recursively, i.e. without using a chart. 

/Advantages/

   * Don't need to specify an ordering.

   * Useful for debugging, can compare directly to recursive function.

/Disadvantages/

   * A ridiculous way to solve a DP. For most non-trivial DPs, this will have exponential complexity.

-}

module Data.DP.Solvers.Recursive (recursive) where

--{{{  
import Data.DP.Internals
import Data.DP.SolverInternal
import Data.Semiring
import qualified Data.Map as M
import Control.Monad.Identity
import Data.DP.SolverAPI
--}}}

newtype Recursive (m :: * -> *) (ch :: * -> * -> *) i c int = Recursive {
      empty :: (ch i int)
    }

recursive :: DPSolverSame monad Recursive M.Map index cell
recursive = mkSolver $ Recursive M.empty

instance (Monad m, Cell cell) => SolveDP (DPSolver m Recursive ch ind cell internal) where
    type Frame (DPSolver m Recursive ch ind cell internal) = ind
    startSolver o (DPSolver (Recursive e)) last dp = do 
      res <- solveNaive last
      return $ DPSolution res e  
        where solveNaive i = reduceBase o (dp i) solveNaive
