{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, TypeFamilies, KindSignatures #-}
module Data.DP.Solver where 

--{{{  
import Data.DP 
import qualified Data.Map as M 
import NLP.Semiring
import Safe 
import Control.Monad.Identity
--}}}

newtype DPBase  (monad :: * -> *) solver   (chart :: * -> * -> *) ind cell internal = DPBase (solver monad chart ind cell internal)

type SameDPBase m solver chart ind cell = DPBase m solver chart ind cell cell

data SolveState = SolveState

class DPSolveBase s where 
    type Chart s :: * -> * -> *
    type DCell s 
    type Ind s  
    type Internal s 
    type DPMonad s :: * -> * 
    
instance (Monad m) => DPSolveBase (DPBase m s ch ind cell int) where 
    type Chart (DPBase m s ch ind cell int) = ch 
    type DCell (DPBase m s ch ind cell int) = cell
    type Ind   (DPBase m s ch ind cell int) = ind
    type Internal   (DPBase m s ch ind cell int) = int
    type DPMonad   (DPBase m s ch ind cell int) = m


class (DPSolveBase s, Monad (DPMonad s)) => DPSolve s where 
    type Frame s
    startSolver :: 
               (DCell s -> (DPMonad s) (DCell s))  -> 
               s -> 
               Frame s ->
               DP (Ind s) (DCell s) -> 
               (DPMonad s) (DPSolution (Chart s) (Ind s) (DCell s) (Internal s))

data DPSolution chart ind cell internal = DPSolution {
      getResult :: cell,
      getChart :: chart ind internal
}

type DPSimpleSolution chart ind val = DPSolution chart ind (Identity val)

-- 

getSimpleResult :: Identity (DPSolution chart ind (Identity r) z) -> r  
getSimpleResult = runIdentity . getResult . runIdentity  

-- 
solveDP solver dp frame = startSolver return solver dp frame  
solveSimpleDP a b c = solveDP a b (fromSimple c)
