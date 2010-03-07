
{-| A compact embedded language for specifying dynamic programs \(DPs\) for optimization.

<http://en.wikipedia.org/wiki/Dynamic_programming>

The style of the language is meant to invoke the formal mathematical 
recursions common in CLRS and other algorithm texts, while still providing 
the flexibility for several styles of solver. 

For solvers, see "Data.DP.Solvers". For example DPs, see "Data.DP.Examples". 

-}

module Data.DP ( 

-- * Terminology
{-|

[Dynamic Program]  an optimization problem where each subsolution is a recursive combination of subsolutions. This property is known as optimal substructure.

[Chart/Table] - A data structure for memoizing the results of subproblems. For simple DPs, the chart is indexed by the subproblem and holds its solution.

[Item]  A pair of (subproblem key, value). 

[Cell] A further division of the chart into sets of items. Useful when there is sparsity of relevant items. Implemented in chart-cell DPs.

[Semiring] Informally, a type paired with a (+) and (*) operation. In Haskell, this is expressed with the 'Semiring' class, we use 'monoid' for plus, and 'multiplicative' for times, thus (+) -> 'mappend', 0 -> 'mempty', (*) -> 'times', 1 -> @'one'@. See the semiring library for a collection of useful semirings.   

This library provides a specification language for DPs defined using arbitrary semirings. It's
goal is to abstract the details of the solver from the mathematical specification of the DP.

There are currently two types of DPs covered by the specification language -
simple and chart-cell. 
-}

  module NLP.Semiring,

-- * Simple

{-|
Simple DPs can be represented entirely by arrays
where each index represents a subproblem and its contents the result of 
the subproblem. In addition to a semiring, simple DPs only have two operations, @f@ and @constant@. 
@f@ references value in the DP chart and @constant@ lifts a semiring value into the DP.  

Perhaps the simplest example is the fibonacci sequence. First, a naive recusive definition -  

> fibNaive 0 = 1
> fibNaive 1 = 1
> fibNaive i = (fibNaive (i-2)) + (fibNaive (i-1))

Now as a DP -

> fib 0 = one
> fib 1 = one
> fib i = (f (i-2)) `mappend` (f (i-1))

Since our definition is not recursive, we replace fib with @f@ so that it can be intercepted by a solver and memoized (or perhaps not)
Other changes are just cosmetic. Notice that we replace the num literals and + with their semiring counterparts. 

If we wanted, add some other literal say. 

> fibNaive i = (fibNaive (i-2)) + (fibNaive (i-1)) + i

We would write - 

> fib i = (fib (i-2)) `mappend` (fib (i-1)) `mappend` (constant i)

The final type is @fib :: SimpleDP Int Counting@, this means that we are indexing on Int and using the Counting semiring
(where + and * are defined as is). Fib can then be used with a solver to produce the final value.  

-}
                 DPSubValue, 
                 f, 
                 constant, 
                 SimpleDP, 

-- * Chart-Cell
{-|
Chart-Cell DPs are a generalization of simple DPs, and are slightly more complicated, 
but can be much efficient in practice when there is sparsity in the sub-structure of the DP. 

For instance, assume that we have a DP for an HMM, indexed on the current position and its current state. 
If we represent this with a simple DP we get a chart of size /O(|pos| * |state|)/. In practice, though |state|
may be very large and very sparse for a given problem. So instead of having a cell for each (pos,state) pair, 
we instead want a cell for each pos containing a set of states. 

Chart-Cell DPs give this kind of representation. They allow you to specify a chart containing 
cells containing sets of items. Each item represents an answer to a sub-problem.  

Here's the HMM example. First, we write it in its Simple form- 

> hmmSimple trans (0, Start) = one
> hmmSimple trans (i, State curState) = 
>     mconcat $ 
>          [ f (i-1, trans ) `times` 
>            constant (lastState `trans` curState) | 
>            lastState <- states] 

Notice how the chart is indexed by position and state, at each index we look back at all possible incoming states. 
So /O(|pos|*|state|)/ indices and /O(|state|)/ at each index.  

Here's the Chart-Cell version -  

> hmmCC allTrans 0 = mkItem Start one
> hmmCC allTrans i = 
>    getCell (i-1) (\(lastState, lastScore) -> 
>        mkCell $ [ mkItem (State newState)
>                   (lastScore `times` constant score) | 
>                  (newState, score) <- allTrans lastState]) 

We now index the chart by just position, and index each cell by the state. This method has the same worst-case complexity, 
but in practice can be much faster since there can be sparsity (either natural or introduced by pruning) at certain states. 
-}
                 DPItem,
                 DPCell,
                 mkItem,
                 mkCell, 
                 Item, 
                 getCell, 
                 DP,
                 -- * Conversion 
                 fromSimple
                 
               ) where 

--{{{  Imports
import qualified Data.Map as M 
import NLP.Semiring
import Safe
import Data.DP.Internals
import Control.Monad.Identity
--}}}

type SimpleDP ind val = ind -> DPSubValue ind (Identity val)

-- | Retrieve the solution of subproblem of the DP for a given index.
--   Note: The ordering and storage of this retrieval is determined by the
--   solver used. 
f :: ind -> DPSubValue ind (Identity val)
f =  DPNode ()

-- | Lift a semiring value into a DPSubValue. 
constant :: CellVal cell -> DPSubValue index cell
constant = Constant


-- | A dynamic program. 
type DP index cell = index -> DPCell index cell 

-- | Groups several items into a cell. Items with the same key are combined with @mappend@
mkCell :: [DPItem index cell] -> DPCell index cell
mkCell = Many

-- | Create a new item. (@'mkItem' key val@) will create an item subindexed by key with value val.mkItem :: CellKey cell -> DPSubValue index cell -> DPItem index cell
mkItem = DPItem 

-- | Lookup a cell from the chart. @'getCell' ind fn@ will lookup index @ind@ and then call @fn@  
--   repeatedly with each item in the cell. It then concats the resulting items, combining 
--   similarly keyed items with @mappend@
--   Use instead of @f@ for Chart-Cell DPs 
getCell
  :: index
     -> (Item index cell -> DPCell index cell)
     -> DPCell index cell
getCell = Request

-- | Convert a Simple DP to a General DP 
fromSimple :: SimpleDP a b -> DP a (Identity b)
fromSimple simple i = mkCell [mkItem () (simple i)] 
