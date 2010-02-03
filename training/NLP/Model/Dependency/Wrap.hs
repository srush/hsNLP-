module NLP.Model.Dependency.Wrap where 
import NLP.Language.SimpleLanguage
import NLP.Grammar.DependencySent
import NLP.WordLattice

newtype DWord = DWord (GWord, Int)
    deriving (Eq, Ord, Show)
instance WordSymbol DWord where 
    getLex (DWord d) = getLex $ fst d
    getPOS (DWord d) = getPOS $ fst d

dwInd (DWord a) = snd a

mkDepWord word ind = DWord (word, ind)

type DSentence = DependencySentence ALabel DWord

dSentence dsent = sentFromArray sent 
        where (DependencySentence sent _) = dsent 