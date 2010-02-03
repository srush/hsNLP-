{-# LANGUAGE TypeFamilies, TypeSynonymInstances, ScopedTypeVariables, FlexibleInstances, GeneralizedNewtypeDeriving #-} 
module NLP.Model.TAG.Semi where 

--{{{  Imports
import Helpers.Common
import NLP.Grammar.TAG
import NLP.Model.TAG.Wrap
import NLP.Model.TAG.Adjunction
import qualified NLP.Semiring.Derivation as D
import NLP.Grammar.Dependency
import NLP.Semiring
import NLP.Semiring.Prob
import NLP.Model.CreateableSemi
import NLP.Probability.Chain
import NLP.Semiring.Viterbi
import NLP.Semiring.ViterbiNBestDerivation
--}}}

data DerivationCell = DerivationCell {
      dcWord :: TWord
} deriving (Eq, Show, Ord)

newtype TAGDerivation = TAGDerivation (ParseDerivation (AdjunctionInfo (DerivationCell))) 
    deriving (Eq, Monoid)

mkDerivationCell word = 
    DerivationCell word 

instance CreateableSemi (CVD TAGDerivation) where 
    type Counter (CVD TAGDerivation) = Double
    type Model (CVD TAGDerivation) = Collins
    mkSemiSmall p = CVD $ viterbiHelp p (TAGDerivation $ Dependency mempty)
    mkSemi p event context = CVD $  
        case childTWord event of 
             Nothing -> 
                 viterbiHelp p (TAGDerivation (Dependency mempty))
             Just child' ->                 
                 viterbiHelp p $ TAGDerivation
                      (singletonDep (parentInd context) (twInd child') $ 
                          (AdjunctionInfo (spinePos context) (fromJustNote "child" $ NLP.Model.TAG.Adjunction.adjType event) (mkDerivationCell child')))   


instance Show (TAGDerivation) where 
    show = render . pPrint

instance Pretty (TAGDerivation) where 
    pPrint (TAGDerivation der) = (text $ show der)
