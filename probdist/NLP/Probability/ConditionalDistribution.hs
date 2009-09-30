{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module NLP.Probability.ConditionalDistribution where 
import qualified Data.Map as M
import qualified Data.IntMap as IM
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
    SumTrie (Sub context) (Observed event)


singletonObservation :: (Context context, Enum event) => event -> context -> CondObserved event context
singletonObservation event context = 
    addColumn decomp observed mempty 
        where observed = singleton event 
              decomp = decompose context 

observedInContext context cond = 
    maybe [] observedEvents $ TW.lookup (decompose context) cond  

class (Ord (Sub a)) => Context a where 
    type Sub a 
    decompose ::  a -> [Sub a]

newtype SimpleContext = SimpleContext (Int, Int) 

instance Context SimpleContext where 
    type Sub SimpleContext = Int
    decompose (SimpleContext (a,b)) = [a,b]


type DistributionTree event context = 
    SumTrie (Sub context) (ExtraObserved event)

data CondDistribution event context = CondDistribution {
       cond :: context -> Distribution event
}
probMLE :: (Enum event) => event -> ExtraObserved event -> Double
probMLE ev exobs =
    assert (total > 0) $  
    (IM.findWithDefault 0.0 (fromEnum ev) c) / total
    where c = counts $ eoObserved exobs 
          total = eoTotal exobs   

estimateWittenBell :: (Enum event, Context context, Show event) =>
                      CondObserved event context -> 
                      CondDistribution event context 
estimateWittenBell = estimateWittenBell_ . fmap storeState 

estimateWittenBell_ :: (Enum event, Context context, Show event) => 
                      --DistributionTree event context -> 
                      DistributionTree event context -> 
                      CondDistribution event context
estimateWittenBell_ cstat = 
    CondDistribution conFun 
    where
      --conFun :: (Context context, Enum event) => context -> Distribution event
      conFun context = (Distribution $ \event ->  
                            assert (not $ isNaN $ wittenBell stats event)  $
                            wittenBell stats event 
                       ) 
          where stats = map (\k -> TW.lookupWithDefault (storeState mempty) k cstat)  $ reverse $  
                        tail $ inits $ decompose context
            
   --   cobsStat :: DistributionTree event context 
      --cobsStat = toDistributionTree cobs 
      
            

  --    wittenBell :: (Enum event) => [ExtraObserved event] -> event -> Double
      wittenBell [last] event =  if eoTotal last > 0 then probMLE event last else 0.0  
      wittenBell (cur:ls) event =  --trace ((show cur) ++ show l) $  
          if eoTotal cur > 0 then ((1-l) * (probMLE  event cur)) + (l * (wittenBell ls event)) 
          else wittenBell ls event  
          where l = lambda cur

      lambda :: (Enum a) => ExtraObserved a -> Double
      lambda eobs = nonTrivial / (nonTrivial + total)
          where total = eoTotal eobs
                nonTrivial = eoUnique eobs
          


estimateConditional est obs =
    CondDistribution $ \context -> 
        fromJustNote ("Context not found: " ++ show context) $ 
        --fromJustDef (uniform 1000000) $
        M.lookup (decompose context) condMap  
    where 
     condMap = M.fromList $ 
               map (\(letters, holder) -> ( letters, est holder)) $ 
               expand_ obs 




data Trigram = Trigram String String
instance Context Trigram where 
    type Sub Trigram = String
    decompose (Trigram w1 w2) = [w2, w1] 
    --compose [w2, w1] = Trigram w1 w2 
    
