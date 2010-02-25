{-# LANGUAGE TypeSynonymInstances, TypeSynonymInstances, TypeFamilies, FlexibleInstances, GeneralizedNewtypeDeriving, UndecidableInstances, TemplateHaskell, MultiParamTypeClasses, BangPatterns, StandaloneDeriving #-}
module NLP.Probability.Chain (simpleObserve,
                              JointModel (..),
                              M2(..), M3(..), M4(..), M5(..), M7(..), HolderPretty, holderPretty, hPretty
                             ) where 
import NLP.Probability.ConditionalDistribution
import NLP.Probability.Distribution 
import NLP.Probability.Observation
import qualified Data.Map as M
import Control.Monad.Identity
import Data.Monoid
import Data.List (intercalate)
import Control.Monad (liftM)
import Data.Binary
import Text.PrettyPrint.HughesPJClass
import Control.DeepSeq

class JointModel a where 
    data FullEvent a
    data FullContext a
    data Probs a
    data Observation a
    data Pairs a 
    chainRule :: FullEvent a -> FullContext a -> Pairs a
    observe :: Pairs a -> Observation a
    prob :: Probs a -> Pairs a -> Prob
    estimate :: Observation a -> Probs a

class Estimate a where 
    type Dist a 

instance Event String where type EventMap String = M.Map
instance Event Int where type EventMap Int = M.Map

data Con1 = Con1 (Int, String)
          deriving (Eq, Ord)

data Con2 = Con2 (String, Int, String)
          deriving (Eq, Ord) 

instance Context Con1 where 
    type Sub Con1 = Con1
    type SubMap Con1 = M.Map
    decompose a = [a]

instance Context Con2 where 
    type Sub Con2 = Con2
    type SubMap Con2 = M.Map
    decompose a = [a]

newtype M2 m a b = M2 (m a, m b)
    deriving (Show, Eq, Ord, Binary, NFData)

newtype M3 m a b c = M3 (m a, m b, m c)
    deriving (Show, Eq, Ord, Binary, NFData )

newtype M4 m a b c d = M4 (m a, m b, m c, m d)
    deriving (Show, Eq, Ord, Binary, NFData)

newtype M5 m a b c d e = M5 (m a, m b, m c, m d, m e)
    deriving (Show, Eq, Ord, Binary, NFData)

newtype M7 m a b c d e f g = M7 (m a, m b, m c, m d, m e, m f, m g)
    deriving (Show, Eq, Ord, Binary, NFData)

class HolderPretty a where 
    holderPretty :: (b -> Doc) -> a b -> Doc

instance HolderPretty Maybe where 
    holderPretty s (Just a) = s a
    holderPretty _ Nothing = empty

hShow :: (HolderPretty m, Show a) => m a -> Doc
hShow = holderPretty  (text .show)

hPretty :: (HolderPretty m, Pretty a) => m a -> Doc
hPretty = holderPretty pPrint

csep = hsep .  punctuate comma . filter (not. isEmpty) 

instance (HolderPretty m, Pretty a, Pretty b, Pretty c, Pretty d, Pretty e, Pretty f, Pretty g) => Pretty (M7 m a b c d e f g) where 
    pPrint (M7 (a, b, c, d, e, f, g)) = csep [hPretty a, hPretty b, hPretty c, hPretty d, hPretty e, hPretty f, hPretty g]

instance (HolderPretty m, Pretty a, Pretty b, Pretty c) => Pretty (M3 m a b c) where 
    pPrint (M3 (a, b, c)) = csep [hPretty a, hPretty b, hPretty c]

instance (Eq a) => Eq (Identity a) where 
    (==) a b = (runIdentity a) ==  (runIdentity b)

instance (Ord a) => Ord (Identity a) where 
    compare a b = (runIdentity a) `compare`  (runIdentity b)

instance (Show a) => Show (Identity a) where 
    show = show . runIdentity

deriving instance (Binary a) => Binary (Identity a)

instance HolderPretty Identity where 
    holderPretty s = s . runIdentity

simpleObserve a b = observe $ chainRule a b 