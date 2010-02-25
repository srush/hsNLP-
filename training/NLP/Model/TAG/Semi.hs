{-# LANGUAGE TypeFamilies, TypeSynonymInstances, ScopedTypeVariables, FlexibleInstances, GeneralizedNewtypeDeriving, FlexibleContexts, StandaloneDeriving #-} 
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
import NLP.Semiring.LogProb
import NLP.Model.CreateableSemi
import NLP.Probability.Chain
import NLP.Semiring.Viterbi
import NLP.Semiring.ViterbiNBestDerivation
import NLP.Grammar.Spine
import NLP.Language.SimpleLanguage
--}}}


data TestData = TestData {td :: NonTerm}
              deriving (Show)
deriving instance Read TestData

data DerivationCell = DerivationCell {
      dcWord :: TWord
} deriving (Eq, Show, Ord, Read)

newtype TAGDerivation = TAGDerivation (ParseDerivation (AdjunctionInfo (DerivationCell))) 
    deriving (Eq, Show, Read, Monoid)

mkDerivationCell word = 
    DerivationCell word 

type Debug = (FullEvent Collins, FullContext Collins, ProbDebug, LogProb)
--type ProbDebug = ([(Double,Double)],
--                  [(Double,Double)],
--                  [(Double,Double)])
type TAGCounter = (FullEvent Collins -> FullContext Collins -> LogProb,
                   FullEvent Collins -> FullContext Collins -> ProbDebug)

instance CreateableSemi (CVDDebug Debug TAGDerivation) where 
    type Counter (CVDDebug Debug TAGDerivation) = TAGCounter 
    type Model (CVDDebug Debug TAGDerivation) = Collins
    --mkSemiSmall p = CVD $ viterbiHelp p (TAGDerivation $ Dependency mempty)
    mkSemi (probs,debug) event context = CVDDebug $  
         case childTWord event of 
           Nothing -> 
               viterbiHelp p (([(event,context,pd,p)],TAGDerivation (Dependency mempty)))
           Just child' ->                 
               viterbiHelp p $ ([(event,context,pd,p)], TAGDerivation
                (singletonDep (parentInd context) (twInd child') $ 
                 (AdjunctionInfo (spinePos context) 
                  (fromJustNote "child" $ NLP.Model.TAG.Adjunction.adjType event) 
                  (mkDerivationCell child'))))
           where p = probs event context
                 pd = debug event context

instance CreateableSemi (CVD TAGDerivation) where 
    type Counter (CVD TAGDerivation) = TAGCounter 
    type Model (CVD TAGDerivation) = Collins

    mkSemi (probs,debug) event context = CVD $  
         case childTWord event of 
           Nothing -> 
               viterbiHelp p (TAGDerivation (Dependency mempty))
           Just child' ->                 
               viterbiHelp p $ TAGDerivation
                (singletonDep (parentInd context) (twInd child') $ 
                 (AdjunctionInfo (spinePos context) 
                  (fromJustNote "child" $ NLP.Model.TAG.Adjunction.adjType event) 
                  (mkDerivationCell child')))
           where p = probs event context



--instance Show (TAGDerivation) where 
--    show = render . pPrint

instance Pretty (TAGDerivation) where 
    pPrint (TAGDerivation der) = (text $ show der)

class (CreateableSemi semi, BestScorer TAGDerivation LogProb semi) => TAGDecodeSemi semi 

instance TAGDecodeSemi (CVD TAGDerivation)
instance TAGDecodeSemi (CVDDebug Debug TAGDerivation)