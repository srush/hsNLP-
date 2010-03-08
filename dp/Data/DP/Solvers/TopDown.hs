{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, TypeFamilies, KindSignatures #-}

{-|

Top-Down Memoization Solver. Solve the DP by doing it recursively, but save the intermediate results so they do not need to be recomputed. 

/Advantages/

   * Lazy, both in a programming sense and in exploring the chart. 

   * Don't need to specify an ordering. 

/Disadvantages/

   * Hard to debug and profile, no specific ordering. 

-}


module Data.DP.Solvers.TopDown (
                                -- * Predefined Solvers
                                topDownMap,
                                topDownGenMap,
                                -- * Custom Solvers                                     
                                -- | You can define a custom TopDown solver by implementing
                                --   the @'TopDown'@ strategy and calling @mkSolver@ 

                                TopDown(..)
                               )

where 

--{{{  Imports
import Data.DP.Internals
import Data.DP.SolverInternal
import Data.Array.IArray
import qualified Data.Map as M
import qualified Data.ListTrie.Base.Map as GM
import Data.Semiring
import Safe
import Control.Monad.State.Strict
import Control.Monad.Identity
import Data.DP.SolverAPI
--}}}

data TopDown m chart ind cell internal  = TopDown {
      td_lookupMaybe :: ind -> chart ind internal -> m (Maybe cell),
      td_insert :: ind -> cell -> chart ind internal -> m (chart ind internal),
      td_empty  :: m (chart ind internal)
    }

instance (Monad m, Cell cell) => SolveDP (DPSolver m TopDown ch ind cell int) where
    type Frame (DPSolver m TopDown ch ind cell int) = ind
    startSolver o (DPSolver solver) last dp  = do
      init <- td_empty solver
      res <- solveNaive last init
      return $ DPSolution res init
        where solveNaive i chart = reduceBaseWrite o (dp i) mylookup chart
              mylookup i = do 
                dat <- get 
                let chart = dpData dat
                look <- lift $ td_lookupMaybe solver i chart 
                case look of 
                  Just a -> return a 
                  Nothing -> 
                      do 
                        res <- lift $ solveNaive i chart
                        newchart <- lift $ td_insert solver i res chart
                        put $ dat{dpData = newchart}
                        return res

topDownGenMap :: (GM.Map map ind, Monad m) => DPSolver m TopDown map ind cell cell
topDownGenMap = mkSolver $ TopDown {
                 td_empty = return $ GM.empty,
                 td_lookupMaybe = (\a b -> return $ GM.lookup a b),
                 td_insert = (\a b c -> return $ GM.insert a b c)
               }

topDownMap :: (Ord ind, Monad m) => DPSolver m TopDown M.Map ind cell cell
topDownMap = topDownGenMap
