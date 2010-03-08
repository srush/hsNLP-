{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, TypeFamilies, KindSignatures, Rank2Types #-}

{-|

Bottom-Up Strict Solver. Solve the DP the way they do it in imperative languages, bottom-up and strict. 

/Advantages/

   * Can use imperative data structures like STArray's and STUArray's which are very fast and
     have low memory overhead

/Disadvantages/
   
   * Not lazy, not pure (although you can use maps instead).

   * Requires an ordering of indices

-}

module Data.DP.Solvers.BottomUpStrict (
                                       -- * Predefined Solvers
                                       bottomUpStrictGenMap,
                                       bottomUpStrictMap,
                                       bottomUpStrictMArray,
                                       bottomUpStrictSTUArray,
                                       -- * Custom Solvers
                                       -- | You can define a custom BottomUpStrict strategy by implementing
                                       --   the @'BottomUpStrict'@ type and calling @mkSolver@ 
                                       BottomUpStrict(..)
                                      ) where 

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
import Data.Array.MArray
import Data.Array.ST
import Data.Array.Unboxed
import Control.Monad.ST
import Data.DP.SolverAPI
--}}}



data BottomUpStrict m chart ind cell internal  = BottomUpStrict {
      bus_lookup :: ind -> chart ind internal -> m (cell),
      bus_insert :: ind -> cell -> chart ind internal -> m (chart ind internal),
      bus_empty  :: m (chart ind internal)
    }

instance (Cell cell, Monad m) => SolveDP (DPSolver m BottomUpStrict  ch ind cell int) where
    type Frame (DPSolver m BottomUpStrict ch ind cell int) = [ind]
    startSolver o (DPSolver solver) order dp  = do
      init <- bus_empty solver
      (val, chart) <- runStateT (last `liftM` mapM manage order) init  
      return $ DPSolution val chart
        where  
          manage i = do 
            chart <- get
            res <- lift $ reduceBase o (dp i) (\a -> bus_lookup solver a chart)
            newchart <- lift $ bus_insert solver i res chart
            put $ newchart
            return $! res


bottomUpStrictGenMap :: (GM.Map map ind) => DPSolverSame Identity (BottomUpStrict ) map ind cell                     
bottomUpStrictGenMap = mkSolver $  BottomUpStrict {
                 bus_empty = return GM.empty,
                 bus_lookup = (\a b -> return $ fromJustNote "" $  GM.lookup a b),
                 bus_insert = (\a b c-> return $ GM.insert a b c)
               }

bottomUpStrictMap :: (Ord ind) => DPSolverSame Identity (BottomUpStrict ) M.Map ind cell                     
bottomUpStrictMap = bottomUpStrictGenMap

bottomUpStrictMArray :: (Ix ind, MArray array int m) => (cell -> int) -> (int -> cell) -> (ind, ind) -> 
                   DPSolver m BottomUpStrict array ind cell int                     
bottomUpStrictMArray toInternal fromInternal bounds = mkSolver $  BottomUpStrict {
                 bus_empty = newArray_ bounds,
                 bus_lookup = (\a b -> fromInternal `liftM` readArray b a),
                 bus_insert = (\i cell chart ->  do {writeArray chart i (toInternal cell); return chart})
               }


bottomUpStrictSTUArray :: (MArray (STUArray s) int (ST s), Ix ind, Cell (Identity cell)) => 
                 (cell -> int) -> (int -> cell) -> (ind, ind) -> 
                 DPSolver (ST s) BottomUpStrict (STUArray s) ind (Identity cell) int
bottomUpStrictSTUArray toInternal fromInternal = bottomUpStrictMArray (\ (Identity a) -> toInternal a) (Identity . fromInternal) 



