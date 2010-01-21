module NLP.TreeBank.TAG (toTAGDependency, toTAGTest, toTAGSentence, SpineExist, tagRoot, root) where 

--{{{  Imports 
import Helpers.Common
import Data.Array
import NLP.Language hiding (mkNonTerm)
import NLP.WordLattice
import NLP.Grammar.TAG hiding (adjPos)
import NLP.Grammar.Spine
import NLP.Grammar.NonTerm

import NLP.Grammar.Dependency
import NLP.TreeBank.TreeBank
import qualified Data.Set as S
import qualified Data.Map as M
import Debug.Trace
import NLP.Model.TAGWrap
--}}}



tagRoot :: (Language l) => (GWord l, TSpine l) 
tagRoot = (GWord (mkWord "ROOT", mkPOS "ROOT"), mkSpine [mkNonTerm "ROOT"])

root:: (Language l) => Int -> (TWord l)
root ind = mkTAGWord  word spine ind 
    where 
    (word,spine) = tagRoot

toTAGSentence :: (Language l) => WordInfoSent l -> Sentence (TWord l)
toTAGSentence (WordInfoSent wis)=  
    mkTAGWords $ (map (\wi -> (GWord (word wi, pos wi), spine wi )) $ elems wis) ++ [tagRoot]

toTAGDependency :: (Language l) => WordInfoSent l -> TSentence l 
toTAGDependency (WordInfoSent wis) = TAGSentence sent depstruct
    where (Sentence sent) = toTAGSentence (WordInfoSent wis)
          depstruct = Dependency $ M.fromList $ map convertWI  $ elems wis
          convertWI wi = (ind wi, 
                          if head == 0 then DEdge n $ AdjunctionInfo (adjPos wi) (sister wi) () 
                          else DEdge head $ AdjunctionInfo (adjPos wi) (sister wi) ())
              where n = latticeLength (Sentence sent)
                    head = adjoinInd wi

type SpineExist l = M.Map (POS l) (S.Set (TSpine l))

mkTAGWords words = 
    mkSentence $ newWords
        where newWords = map (\(i, (a,b)) -> mkTAGWord a b i) $ zip [1..] words

mkTestTAGWord :: (Language l) => SpineExist l -> (Int, GWord l) -> [TWord l]
mkTestTAGWord counts (ind, GWord (word,pos)) = 
     map (\sp -> mkTAGWord (GWord (word,pos)) sp ind) $ S.toList $ 
     fromJustDef mempty $ M.lookup pos counts

toTAGTest counts (WordInfoSent wis) = sent
      where sent = mkSentenceLat $ 
                   (map (mkTestTAGWord counts) $ 
                    map (\wi -> (ind wi, GWord (word wi, pos wi))) $ elems wis) ++ [[root (n+1)]]
            (_,n) = bounds wis

-- toSentence :: WordInfoSent l -> Sentence (GWord l)
-- toSentence (WordInfoSent wis)=  
--     mkSentence $ map (\wi -> (word wi, pos wi)) $ elems wis

-- liftCommas (WordInfoSent wis) =
--     WordInfoSent $ foldr lift wis commas  
--     where 
--       (_,n) = bounds wis
--       commas = catMaybes $ map (\(i,w)-> if isPOSComma $ pos w then Just i else Nothing) $ zip [1..] $ elems wis 
--       lift i wis = if grandParentInd /= 0 && canLift then lift i (wis // [(i, (wis ! i) {adjoinInd = grandParentInd,
--                                                                adjPos = adjPos parent})])
--                else wis 
--               where
--                 canLift = all ((/= parentInd). adjoinInd . (wis !)) posSiblings
--                 posSiblings  = if i < parentInd then [i-1,i-2..1] 
--                                else [i+1 .. n]
--                 parentInd = adjoinInd (wis ! i)
--                 parent = wis ! parentInd 
--                 grandParentInd = adjoinInd parent
