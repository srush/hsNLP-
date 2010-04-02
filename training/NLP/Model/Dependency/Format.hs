module NLP.Model.Dependency.Format where 

import NLP.ParseMonad
import NLP.Model.Dependency.Wrap
import NLP.Language.SimpleLanguage
import Helpers.Common
import NLP.TreeBank.TreeBank
import Data.Array
import NLP.WordLattice
import NLP.Grammar.DependencySent
import NLP.Grammar.Dependency
import qualified Data.Map as M

depRoot = do 
  aroot <- toAtom $ mkWord "ROOT"
  apos <- toAtom $ mkPOS "ROOT"
  return (GWord (aroot, apos))

root :: Int -> ParseMonad DWord
root ind = do 
  word <- depRoot
  return $ mkDepWord word ind

toDepSentence :: WordInfoSent  -> ParseMonad (Sentence (DWord))
toDepSentence (WordInfoSent wis)= do
  dr <- depRoot
  return $ mkSentence $ zipWith mkDepWord ((map (\wi -> (GWord (word wi, head $ pos wi))) $ elems wis) ++ [dr]) [1..]


type Labeler = (WordInfo -> Maybe WordInfo -> ALabel)

toDependency :: Labeler -> WordInfoSent -> ParseMonad DSentence 
toDependency labeler (WordInfoSent wis)  = do
  (Sentence sent) <- toDepSentence (WordInfoSent wis)
  return $ DependencySentence sent (depstruct sent)
    where 
          depstruct sent = Dependency $ M.fromList $ map (convertWI sent)  $ elems wis
          convertWI sent wi = (ind wi, 
                          if head == 0 then DEdge n $ labeler wi Nothing 
                          else DEdge head $ labeler wi (Just $ wis ! head))
              where n = latticeLength (Sentence sent)
                    head = adjoinInd wi
