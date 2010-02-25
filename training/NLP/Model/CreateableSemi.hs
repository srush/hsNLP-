{-# LANGUAGE TypeFamilies, TypeSynonymInstances, ScopedTypeVariables, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-} 
module NLP.Model.CreateableSemi where 

--{{{  Imports
import Helpers.Common hiding (Derivation)
import NLP.Semiring
import qualified NLP.Semiring.Derivation as D
import NLP.Semiring.LogProb
import NLP.Semiring.Prob
import NLP.Semiring.Viterbi
import NLP.Semiring.ViterbiNBestDerivation
import NLP.Grammar.Dependency
import NLP.Language.SimpleLanguage
import NLP.Probability.Chain
--}}}

instance (Pretty d, Monoid d ) => Pretty (D.Derivation d) where
    pPrint = pPrint . D.fromDerivation 

-- type TAGCountSemi = Derivation TAGCounts 

type ParseDerivation a = Dependency a 

class (Semiring a, Show a) => CreateableSemi a where 
    type Counter a
    type Model a 
--    mkSemi ::  Counter a -> TWord -> (Maybe (TWord)) -> Int -> AdjunctionType -> a
    mkSemi :: Counter a -> FullEvent (Model a) -> FullContext (Model a) -> a
    --mkSemiSmall :: Counter a -> a

newtype CProb model = CProb Prob
    deriving (Monoid, Multiplicative, Semiring, Show)


instance CreateableSemi (CProb model) where
    type Counter (CProb model) = FullEvent model -> FullContext model -> Double
    type Model (CProb model) = model
    mkSemi p e c  = CProb $ Prob $ p e c
    --mkSemiSmall p = CProb $ Prob p 

newtype CD monoid model = CD (D.Derivation monoid)
    deriving (Monoid, Multiplicative, Semiring, Pretty, Show)

instance (Show monoid , Monoid monoid) => CreateableSemi (CD monoid model) where 
    type Model (CD monoid model) = model
    type Counter (CD monoid model) = FullEvent model -> FullContext model -> monoid 
    --mkSemiSmall p = CD $ D.mkDerivation p
    mkSemi p e c = CD $ D.mkDerivation $ p e c


viterbiHelp prob tdep = 
    mkViterbi $ Weighted (prob, D.mkDerivation tdep)

newtype CVD der = CVD (ViterbiDerivation LogProb der)
    deriving (Show, Monoid, Multiplicative, Semiring, BestScorer der LogProb)

newtype CVDDebug a der = CVDDebug (ViterbiDerivation LogProb ([a],der))
    deriving (Show, Monoid, Multiplicative, Semiring)

instance (Monoid der) => BestScorer der LogProb (CVDDebug a der)  where 
    getBestDerivation (CVDDebug c) = snd $ getBestDerivation c 
    getBestScore (CVDDebug c) = getBestScore c

getDebug (CVDDebug c) = fst $ getBestDerivation c 


