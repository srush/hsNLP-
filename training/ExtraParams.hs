{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, ExistentialQuantification #-}
module ExtraParams where
import qualified Data.Map as M
import Helpers.Common
import NLP.Probability.ConditionalDistribution
import NLP.Probability.Distribution 
import NLP.Probability.Observation
import NLP.Probability.EM
import Data.Binary hiding (Word)
import NLP.Grammar.NonTerm
import NLP.Grammar.Spine
import NLP.Grammar.TAG
import NLP.Language.WordLattice
import NLP.Language
import Helpers.MkEnum
import Control.Monad.Random
import Test.QuickCheck.Gen
newtype SideEvent = SideEvent AdjunctionSide 
    deriving (Eq, Ord, Bounded, Binary, Enum, Show)

instance Event SideEvent where
    type EventMap SideEvent = M.Map

data SideContext l = SideContext {
      spine :: Spine l,
      word  :: GWord l, 
      engSide  :: AdjunctionSide 
}

instance (Language l) => Enum (SideContext l) where 
    fromEnum sc = mkFromEnum3 (spine sc, maxBound) 
                              (word sc, maxBound) 
                              (engSide sc, maxBound) 
    toEnum n = SideContext a b c 
        where (a,b ,c)  = mkToEnum3 (maxBound, maxBound, maxBound) n

instance (Language l) => Bounded (SideContext l) where 
   minBound = SideContext minBound minBound minBound 
   maxBound = SideContext maxBound maxBound maxBound

instance (Language l) => Arbitrary (SideContext l)  where 
     arbitrary = return SideContext `ap` arbitrary `ap` arbitrary `ap` arbitrary 


newtype ManyContexts l = ManyContexts [SideContext l]
    

instance (Language l) => Arbitrary (ManyContexts l) where 
    arbitrary = ManyContexts `liftM` vectorOf 100000 arbitrary


data SideSubContext l= SideSubContext {
      mspine :: Maybe (Spine l),
      mengSide :: Maybe AdjunctionSide,
      mword :: Maybe (GWord l)
    } deriving (Show)

instance (Language l) => (Eq (SideSubContext l)) 
instance (Language l) => (Ord (SideSubContext l)) 
instance (Language l) => Pretty (SideSubContext l) where 
    pPrint = text . show


subDef = SideSubContext Nothing Nothing Nothing

instance (Language l) => Context (SideContext l) where 
    type SubMap (SideContext l) = M.Map 
    type Sub (SideContext l) = (SideSubContext l) 
    decompose sc = [subDef {mspine = Just $ spine sc,
                            mengSide = Just $ engSide sc},
                    subDef {mword = Just $ word sc}]

newtype FlipCounts l = 
    FlipCounts { flipCounts :: CondObserved SideEvent (SideContext l)}
    deriving (Monoid)

data FlipProbs l = FlipProbs { flipProbs :: CondDistribution SideEvent (SideContext l)}
  
randomInitial :: (Language l) => FlipProbs l
randomInitial = estimateFlipProbs $ 
  FlipCounts $ (evalRand (randomCondCounts mc) std)
    where std = mkStdGen 1 
          ManyContexts mc = unGen arbitrary std 0

estimateFlipProbs ::  (Language l) => FlipCounts l -> FlipProbs l 
estimateFlipProbs = FlipProbs . estimateGeneralLinear (wittenBell 5) . flipCounts  

testProbs = FlipProbs (\context (SideEvent side) -> 
                           if engSide context == side then 1.0 else 0.0 ) 

probFlip probs side context = 
    (flipProbs probs) context (SideEvent side)

mkFlip word side = 
    SideContext { word = (twWord word),
                  spine = twSpine word,
                  engSide = side}