{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, TypeFamilies, KindSignatures #-}

{-|

Bottom-Up Lazy Solver. Solve the DP in an ordered bottom up fashion, but compute values lazily. 

/Advantages/

   * Some of the laziness benefits of a top-down method.

   * Lets you use efficient, pure data structures, in particular "Data.Array".


/Disadvantages/

   * No threaded monad (uses identity).

   * Cannot use UArrays (not lazy)

   * Requires an ordering of indices
-}

module Data.DP.Solvers.BottomUpLazy (
                                     -- * Predefined Solvers
                                     bottomUpLazyGenMap,
                                     bottomUpLazyMap,
                                     bottomUpLazyIntMap,
                                     bottomUpLazyIArray,
                                     bottomUpLazyArray,
                                     -- * Custom Solvers
                                     -- | You can define a custom BottomUpLazy solver by implementing
                                     --   the @'BottomUpLazy'@ strategy and calling @mkSolver@ 
                                     BottomUpLazy (..)
                                    ) where 


--{{{  Imports
import Data.DP.Internals
import Data.DP.SolverInternal
import Data.Array.IArray
import Data.Array.Unboxed
import qualified Data.Map as M
import qualified Data.ListTrie.Base.Map as GM
import Data.Semiring
import Safe
import Control.Monad.Identity
import Control.Monad.State.Strict
import Data.DP.SolverAPI
--}}}

data BottomUpLazy m chart ind cell internal  = BottomUpLazy {
      bul_lookup :: ind -> chart ind internal -> cell,
      bul_create :: [(ind, cell)] -> (chart ind internal)
    }

type LazyOrderedSolver m chart ind cell = DPSolverSame m BottomUpLazy chart ind cell 

instance ( Cell cell) => SolveDP (DPSolver Identity BottomUpLazy chart ind cell internal ) where
    type Frame (DPSolver Identity BottomUpLazy chart ind cell internal) = [ind]
    startSolver o (DPSolver solver) ordering dp = do
      let chart = (bul_create solver) $  map (\i -> (i, runIdentity $ reduceBase o (dp i) (\a -> return ((bul_lookup solver) a chart)))) ordering
      let res = (bul_lookup solver) (last ordering) chart 
      return $ DPSolution res chart 

              
bottomUpLazyGenMap :: (GM.Map map ind) => DPSolverSame m BottomUpLazy map ind cell  
bottomUpLazyGenMap = mkSolver $  BottomUpLazy {
                     bul_create = GM.fromList,
                     bul_lookup = (\a b-> fromJustNote "lookup failed" $  GM.lookup a b)
                   }
 
bottomUpLazyMap :: (Ord ind) =>  DPSolverSame m BottomUpLazy M.Map ind cell   
bottomUpLazyMap = bottomUpLazyGenMap

bottomUpLazyIntMap :: (Enum ind) => DPSolverSame m BottomUpLazy GM.WrappedIntMap ind cell   
bottomUpLazyIntMap = bottomUpLazyGenMap

bottomUpLazyIArray :: (Cell cell, IArray array int, Ix ind) => 
                  (cell -> int) -> (int -> cell) -> (ind, ind) -> 
                  DPSolver m BottomUpLazy array ind cell int   
bottomUpLazyIArray toInternal fromInternal bounds = mkSolver $ BottomUpLazy {
                                   bul_create = array bounds . map (\(a,b)-> (a,toInternal b))  , 
                                   bul_lookup = (\a b -> fromInternal (b ! a)) 
                                 }

bottomUpLazyArray :: (Ix ind, Cell cell) => (ind, ind) -> DPSolverSame m BottomUpLazy Array ind cell   
bottomUpLazyArray = bottomUpLazyIArray id id


