{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns #-}
module Data.DP where 

--{{{  Imports
import qualified Data.Map as M 
import Data.Array 
import Data.Monoid
import NLP.Semiring
import NLP.Semiring.Counting
import NLP.Semiring.Prob
import NLP.Semiring.Viterbi
import NLP.Semiring.Derivation
import NLP.Semiring.ViterbiNBestDerivation
import Control.Applicative
import Safe
import Control.Monad.State.Strict
import Control.Monad
import Data.List
import Debug.Trace
--}}}

--{{{  DP Structure

-- getInd :: DPMonad ind cell ind  
-- getInd = ind `liftM` get 

-- getCells :: ind -> DPMonad ind cell [cell] 
-- getCells = (cells `liftM` get) 

-- newtype DPMonad ind val a = DPMonad (State (DPState ind) a)
--     deriving (Monad, MonadState (DPState ind))

type DP ind cell val = ind -> DPComplex ind cell val
type SimpleDP ind val = ind -> DPTree ind () val

-- instance (Monoid value) => Monoid (DPMonad index value) where 
--      mappend = lift2 mappend
--      mempty  = return zero

-- instance (Multiplicative value) => Multiplicative (DPMonad index value) where 
--      times = lift2 times
--      one  = return one

data DPOpt = Plus | Times

optFunc Plus = mappend
optFunc Times = times

data DPComplex index cell value = 
    Request index (cell ->  DPComplex index cell value) | 
    Many [DPComplex index cell value] |
    Name cell (DPTree index cell value) 

data DPTree index cell value = 
    DPNode cell index | 
    Constant value | 
    Opt DPOpt (DPTree index cell value) (DPTree index cell value) 

instance (Monoid value) => Monoid (DPTree index cell value) where 
    mappend = Opt Plus
    mempty  = Constant mempty

instance (Multiplicative value) => Multiplicative (DPTree index cell value) where 
    times = Opt Times
    one  = Constant one

f = DPNode ()
chart = DPNode
getCells = Request 
constant = Constant

--}}}

data DPState ind cell val = DPState { 
     dpLookup :: ind -> M.Map cell val
}

findCells ind = do 
  state <- get
  return $ dpLookup state ind 

reduceComplex (Name n a) = do
  ared <- reduce a 
  return [(n, ared)]  
reduceComplex (Many a) = do
  rs <- mapM reduceComplex a
  return $ concat rs
reduceComplex (Request ind fn) = do
  cells <- findCells ind
  reduceComplex $ Many $ map fn $ M.keys cells

reduce (Constant a)  = return a
reduce (Opt opt a b) = (liftM2 $ optFunc opt) (reduce a) (reduce b)
reduce (DPNode n i)   = do 
  cells <- findCells i
  return $ fromJustNote "cell" $ M.lookup n cells


solveDPStrict :: (Semiring val, Ord cell) => MemoDPChart chart ind (M.Map cell val) -> SolveDPOrder chart ind cell val   
solveDPStrict solver dp ordering = runState (last `liftM` mapM manage ordering) (mdc_empty solver)     
    where  
      manage i = do 
        chart <- get
        let res = M.fromListWith mappend $ evalState (reduceComplex $ dp i) (DPState (\a -> fromJustNote "" $ ((mdc_lookupMaybe solver) a chart)))
        put $ (mdc_insert solver) i res chart
        return res
      
solveDPOrdered :: (Semiring val, Ord cell) => OrderedDPChart chart ind (M.Map cell val) -> SolveDPOrder chart ind cell val   
solveDPOrdered solver dp ordering = ((odc_lookup solver) (last ordering) chart, chart) 
    where chart = 
              (odc_create solver) $ do 
                i <- ordering
                let res = evalState (reduceComplex $ dp i) (DPState (\a -> ((odc_lookup solver) a chart)))
                return $ (i, M.fromListWith mappend res) 
 
data OrderedDPChart chart ind val  = OrderedDPChart {
      odc_lookup :: ind -> chart ind val -> val,
      odc_create :: [(ind, val)] -> chart ind val
    }

type SolveDPOrder chart ind cell val = DP ind cell val -> [ind] -> (M.Map cell val, chart ind (M.Map cell val)) 

arrayOrderedChart bounds = OrderedDPChart {
                     odc_create = array bounds, 
                     odc_lookup = (\a b -> b ! a) 
                   }

mapOrderedChart :: (Ord ind) => OrderedDPChart M.Map ind val  
mapOrderedChart = OrderedDPChart {
                     odc_create = M.fromList,
                     odc_lookup = (\a b -> (M.!) b a)
                   }

data MemoDPChart chart ind val  = MemoDPChart {
      mdc_lookupMaybe :: ind -> chart ind val -> Maybe val,
      mdc_insert :: ind -> val -> chart ind val -> chart ind val,
      mdc_empty  :: chart ind val 
    }

type SolveDPMemo chart ind cell val = DP ind cell val -> ind -> (val, chart ind val) 
 
mapMemoChart :: (Ord ind) => MemoDPChart M.Map ind val                     
mapMemoChart = MemoDPChart {
                 mdc_empty = M.empty,
                 mdc_lookupMaybe = M.lookup,
                 mdc_insert = M.insert
               }

wrapSimple :: SimpleDP a b -> DP a () b
wrapSimple simple i = Name () (simple i) 

fib :: SimpleDP Int Counting  
fib 0 = one
fib 1 = one
fib i = (f (i-2)) `mappend` (f (i-1))

type Bigram = (String, String)

bigrams = 
   [(("a", "b"), 0.5), 
    (("b", "b"), 0.4), 
    (("b", "a"), 0.3), 
    (("c", "d"), 0.2), 
    (("d", "c"), 0.1),
    (("SOS", "a"), 0.2) 
   ] 

bigramsStartWith word =
    filter ((== word) . fst. fst ) bigrams 

ngram :: DP Int String (ViterbiDerivation Prob [Bigram])
ngram 0 = Name "SOS" $ constant one
ngram i = 
    Request (i-1) (\word -> 
        Many $ do
          (bigram,score) <- bigramsStartWith word 
          return $ Name (snd bigram) $ 
                 ((chart word (i-1)) `times` (constant $ mkViterbi $ Weighted (Prob score, mkDerivation [bigram]))))

data NMap key val = NMap Int (M.Map key val)
    deriving (Show)

nmapInsert :: (Ord key) =>  key -> val -> NMap key val -> NMap key val 
nmapInsert a v (NMap n m) = NMap n $ M.insert a v (if M.size m == n then M.delete (fst $ M.findMin m) m else m)  

nmapLookupMaybe a (NMap _ m) = Just $  (M.!) m a


nmapMemo n = MemoDPChart {
                mdc_insert = nmapInsert,
                mdc_empty = NMap n M.empty,
                mdc_lookupMaybe = nmapLookupMaybe
              }



newtype MaxInt = MaxInt Int
    deriving (Show)  

instance Monoid MaxInt where 
    mappend (MaxInt a) (MaxInt b) = MaxInt $ max a b
    mempty = MaxInt 0

instance Multiplicative MaxInt where 
    one = MaxInt 1
    times (MaxInt a) (MaxInt b) = MaxInt (a + b) 

instance Semiring MaxInt


-- --     primeDP basis ixs = do
-- --       dpState <- get 
-- --       let dp = fn dpState
-- --       let chart = createChart $ basis ++ [evalDP dp chart i | i <- ixs] 
-- --       put $ dpState {chart = chart}
-- --       return ()
-- --           where evalDP dp@(DP s) chart i =  evalState s (DPState i dp chart )
                
                  
-- instance (DPMemo chart ind) => MonadDP (DP chart ind val) chart ind where 
    
--     f j = do       
--       dpState <- get
--       case memoLookup j (chart dpState) of 
--         Nothing -> do 
--           put $ dpState {curIndex = j}
--           v <- fn dpState  
--           DPState {chart = m'} <- get
--           put $ dpState {chart = M.insert j v m'}
--           return v
--         Just v -> 
--           return v

-- -- runChartDP :: (MonadDP dp) => dp -> [(Index dp, Val dp)] -> [Index dp] -> dp (Val dp)      
-- -- runChartDP boundaries order = evalState dp    
-- --     where chart = chartBasis 
          


-- combine :: (MonadDP m, Semiring s) => m s -> m s -> m s
-- combine a b = do 
--   a' <- a
--   b' <- b
--   return $ a' `times` b' 


-- merge :: (MonadDP m, Semiring s) => m s -> m s -> m s
-- merge a b = do 
--   a' <- a
--   b' <- b
--   return $ a' `mappend` b' 

-- (-*-) :: (MonadDP m, Semiring s) => m s -> m s -> m s
-- (-*-) = combine
-- (-+-) :: (MonadDP m, Semiring s) => m s -> m s -> m s
-- (-+-) = merge

-- const ::(MonadDP m) => s -> m s
-- const = return 