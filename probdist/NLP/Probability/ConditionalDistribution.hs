{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module NLP.Probability.ConditionalDistribution where 
import qualified Data.IntMap as IM
import qualified Data.ListTrie.Base.Map as M
import Data.List (inits)
import Data.Monoid
import NLP.Probability.TrieWrap
import qualified NLP.Probability.TrieWrap as TW
import NLP.Probability.Distribution
import NLP.Probability.Observation
import Safe (fromJustNote, fromJustDef)
import Control.Exception
import Debug.Trace 

type CondObserved event context = 
    SumTrie (SubMap context) (Sub context) (Observed event)


singletonObservation :: (Context context, Enum event) => 
                        event -> context -> CondObserved event context
singletonObservation event context = 
    addColumn decomp observed mempty 
        where observed = singleton event 
              decomp = decompose context 

observedInContext context cond = 
    maybe [] observedEvents $ TW.lookup (decompose context) cond  

class (M.Map (SubMap a) (Sub a)) => Context a where 
    type Sub a 
    type SubMap a :: * -> * -> * 
    decompose ::  a -> [Sub a]

-- newtype SimpleContext = SimpleContext (Int, Int) 

-- instance Context SimpleContext where 
--     type SubMap = WrappedInt
--     type Sub SimpleContext = Int
--     decompose (SimpleContext (a,b)) = [a,b]


type DistributionTree event context = 
    SumTrie (SubMap context) (Sub context) (ExtraObserved event)

data CondDistribution event context = CondDistribution {
       cond :: context -> Distribution event,
       condDebug :: context -> (event -> [(Double, Double)]) 
}

probMLE :: (Enum event) => event -> ExtraObserved event -> Double
probMLE ev exobs =
    --assert (total > 0) $  
    (IM.findWithDefault 0.0 (fromEnum ev) c) / total
    where c = counts $ eoObserved exobs 
          total = eoTotal exobs   


lambdaWB :: (Enum a) => ExtraObserved a -> Double
lambdaWB eobs = nonTrivial / (nonTrivial + total)
    where total = eoTotal eobs
          nonTrivial = eoUnique eobs


-- Page 18 of Collins 2003
lambdaWBC :: (Enum a) => ExtraObserved a -> Double
lambdaWBC eobs = total / ((5 * distinct) + total)
    where total = eoTotal eobs
          distinct = eoUnique eobs


estimateWittenBell :: (Enum event, Context context, Show event) =>
                      CondObserved event context -> 
                      CondDistribution event context 
estimateWittenBell = estimateWittenBell_ . fmap storeState 

estimateWittenBell_ :: (Enum event, Context context, Show event) => 
                      --DistributionTree event context -> 
                      DistributionTree event context -> 
                      CondDistribution event context
estimateWittenBell_ cstat = 
    CondDistribution (fst . conFun)  (snd . conFun) 
    where
      --conFun :: (Context context, Enum event) => context -> Distribution event
      conFun context = (Distribution $ (\event -> sum $ (uncurry $ zipWith (*)) $ unzip $ wittenBell stats event 1.0),   
                            -- assert (not $ isNaN $ wittenBell stats event)  $
                        (\event -> wittenBell stats event 1.0)
                       ) 
          where stats = map (\k -> TW.lookupWithDefault (storeState mempty) k cstat)  $ reverse $  
                        tail $ inits $ decompose context
      
            
  --    wittenBell :: (Enum event) => [ExtraObserved event] -> event -> Double
      wittenBell [last] event mult =  if eoTotal last > 0 then [(mult, probMLE event last)] else [(0.0,0.0)]  
      wittenBell (cur:ls) event mult =  --trace ((show cur) ++ show l) $  
          if eoTotal cur > 0 then (l*mult, (probMLE  event cur)) : wittenBell ls event ((1-l)*mult) 
          else wittenBell ls event mult  
          where l = lambdaWBC cur

          
estimateLinear :: (Enum event, Context context, Show event) =>
                      [Double] ->
                      CondObserved  event context -> 
                      CondDistribution event context 
estimateLinear interp = estimateLinear_ interp . fmap storeState 

estimateLinear_ :: (Enum event, Context context, Show event) => 
                      [Double] ->
                      DistributionTree event context -> 
                      CondDistribution event context
estimateLinear_ interpolation cstat = 
    CondDistribution conFun (\_ -> undefined)
    where
      --conFun :: (Context context, Enum event) => context -> Distribution event
      conFun context = (Distribution $ \event ->  
                             sum $ zipWith (*) interpolation $ map (probMLE event) stats  
                       ) 
          where stats = map (\k -> TW.lookupWithDefault (storeState mempty) k cstat)  $ reverse $  
                        tail $ inits $ decompose context
               


-- estimateConditional est obs =
--     CondDistribution $ \context -> 
--         fromJustNote ("Context not found: " ++ show context) $ 
--         --fromJustDef (uniform 1000000) $
--         M.lookup (decompose context) condMap  
--     where 
--      condMap = M.fromList $ 
--                map (\(letters, holder) -> ( letters, est holder)) $ 
--                expand_ obs 




-- data Trigram = Trigram String String
-- instance Context Trigram where 
--     type Sub Trigram = String
--     decompose (Trigram w1 w2) = [w2, w1] 
--     --compose [w2, w1] = Trigram w1 w2 
    
