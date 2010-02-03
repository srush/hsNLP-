module NLP.Model.Dependency.Derivation where 
import NLP.Model.Dependency.Semi
import NLP.Model.ParseTree
import NLP.Language.SimpleLanguage
import NLP.ParseMonad
import NLP.Grammar.Dependency
import NLP.TreeBank.Label

depDerToTree :: DependencyDerivation -> ParseMonad PTree 
depDerToTree (DependencyDerivation tagdep ) =
  return $ ParseTree $ fmap (maybe (Left (Atom 50)) (Left . convertLabeltoNT)) $ convertToTree tagdep