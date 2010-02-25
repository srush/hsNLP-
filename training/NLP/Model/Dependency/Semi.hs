{-# LANGUAGE TypeFamilies, FlexibleInstances, GeneralizedNewtypeDeriving #-} 
module NLP.Model.Dependency.Semi where 
import NLP.Model.CreateableSemi
import NLP.Model.Dependency.Model
import Helpers.Common
import NLP.Language.SimpleLanguage
import NLP.Grammar.Dependency
import NLP.Model.Dependency.Wrap
import NLP.Semiring.LogProb
import NLP.Probability.Chain
newtype DependencyDerivation = DependencyDerivation (ParseDerivation (ALabel)) 
    deriving (Eq, Monoid)

instance Show (DependencyDerivation) where 
    show = render . pPrint

instance Pretty (DependencyDerivation) where 
    pPrint (DependencyDerivation der) = text $ show der

instance CreateableSemi (CVD DependencyDerivation) where 
    type Counter (CVD DependencyDerivation) = FullEvent FirstOrderDep -> FullContext FirstOrderDep -> LogProb
    type Model (CVD DependencyDerivation) = FirstOrderDep
    --mkSemiSmall p = CVD $ viterbiHelp p (DependencyDerivation mempty)
    mkSemi p event context = 
        CVD $ viterbiHelp (p event context) $ DependencyDerivation $ singletonDep (parentInd context) (childInd event) (childLabel event) 