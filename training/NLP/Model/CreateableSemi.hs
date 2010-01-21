{-# LANGUAGE TypeFamilies, TypeSynonymInstances, ScopedTypeVariables, FlexibleInstances, GeneralizedNewtypeDeriving #-} 
module NLP.Model.CreateableSemi where 

--{{{  Imports
import Helpers.Common hiding (Derivation)
import NLP.Semiring
import NLP.Semiring.Derivation
import NLP.Semiring.Prob
import NLP.Semiring.Viterbi
import NLP.Semiring.ViterbiNBestDerivation
import NLP.Grammar.TAG
import NLP.Grammar.Dependency
import NLP.Language
import NLP.Model.TAGWrap
--}}}


instance (Pretty d, Monoid d ) => Pretty (Derivation d) where
    pPrint = pPrint . fromDerivation 
    
-- type TAGCountSemi = Derivation TAGCounts 

data DerivationCell l = DerivationCell {
      dcWord :: TWord l
} deriving (Eq, Show, Ord)

mkDerivationCell word = 
    DerivationCell word 

newtype TAGDerivation l = TAGDerivation (Dependency (AdjunctionInfo (DerivationCell l))) 
    deriving (Eq, Monoid)


viterbiHelp prob tdep = 
    mkViterbi $ Weighted (Prob prob, mkDerivation $ TAGDerivation tdep)

class CreateableSemi a where 
    type Counter a
    mkSemi ::  Counter a -> TWord l -> (Maybe (TWord l)) -> Int -> AdjunctionType -> a l
    mkSemiSmall :: Counter a -> a l

newtype CProb l = CProb Prob
    deriving (Monoid, Multiplicative, Semiring)

instance CreateableSemi CProb where
    type Counter CProb = Double
    mkSemi p head child pos atype  = CProb $ Prob p
    mkSemiSmall p = CProb $ Prob p 

newtype CVD l = CVD (ViterbiDerivation Prob (TAGDerivation l))
    deriving (Show, Monoid, Multiplicative, Semiring)

getCVDBestScore (CVD bs) =  getBestScore bs
getCVDBestDerivation (CVD bs) =  getBestDerivation bs


instance CreateableSemi CVD where 
    type Counter CVD = Double
    mkSemiSmall p = CVD $ viterbiHelp p (Dependency mempty)
    mkSemi p head child pos atype = CVD $  
        case child of 
             Nothing -> 
                 viterbiHelp p (Dependency mempty)
             Just child' ->                 
                 viterbiHelp p
                      (singletonDep (twInd head) (twInd child') $ 
                          (AdjunctionInfo pos atype (mkDerivationCell child')))   

newtype CD m l = CD (Derivation m)
    deriving (Monoid, Multiplicative, Semiring, Pretty, Show)

instance (Monoid m) => CreateableSemi (CD m) where 
    type Counter (CD m) = m
    mkSemiSmall p = CD $ mkDerivation p
    mkSemi p _ _ _ _= CD $ mkDerivation p

instance (Language l) => Show (TAGDerivation l) where 
    show = render . pPrint

instance (Language l) => Pretty (TAGDerivation l) where 
    pPrint (TAGDerivation der) = (text $ show der)
