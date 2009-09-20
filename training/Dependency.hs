module Dependency where 
import qualified Data.Tree as T
import NLP.ChartParse 
import NLP.ChartParse.Eisner
import Sentence
import qualified Data.Map as M
import Data.Array
import NLP.Semiring.Derivation
import NLP.FSM.Simple
import Data.List (partition) 
import Test.QuickCheck
import NLP.Semiring
import DependencyStructure
-- Root is index 0

import Debug.Trace.Helpers
import Debug.Trace

data DependencySentence semi = 
    DependencySentence (Sentence semi GWord) (Dependency ())
                         deriving (Eq)

instance (Semiring semi) => Arbitrary (DependencySentence semi) where     
    arbitrary = do 
      sent <- arbitrary 
      dmap <- arbDepMap (sentenceLength sent) (const arbitrary)
      return $ DependencySentence sent dmap
 
instance (Semiring semi) => Show (DependencySentence semi) where 
    show (DependencySentence sent dep)= T.drawTree $ convertToTree dep (getWord sent)  


--data DependencyState = Start | Mid Int | Finish 
--                     deriving Ord 
sentenceFSMs :: (Semiring semi) =>  
    DependencySentence semi ->  GetSemi GWord () semi ->
    [(DependencyFSM semi, DependencyFSM semi)]     
sentenceFSMs (DependencySentence sent dep) mkSemi = 
    map (\(i, e) -> makeDependencyFSM (getWord sent) mkSemi i e) $
    flattenDep dep

type Counts event = Derivation (M.Map event Int)


type DependencyFSM semi = GraphWFSM Int GWord semi
type GetSemi word edge semi =  word -> word -> (Int, DEdge edge)  -> semi

mkDepDerivation :: GetSemi GWord () (Derivation (Dependency ()))
mkDepDerivation _ _ (head, DEdge child _ )  =
    mkDerivation $ Dependency $ M.singleton child (DEdge head ())

-- prop_dependencyId dep =  
--     dep == (DependencySentence sent (fromDerivation $ finalsemi))
--     where
--       (DependencySentence sent _) = dep
--       fsms = sentenceFSMs dep mkDepDerivation 
--       getFSM i word = fsms !! (i-1)
--       (Just (finalsemi), _) = eisnerParse getFSM sent
--       types = (dep :: DependencySentence (Derivation (Dependency ())))


