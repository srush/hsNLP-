module NLP.Model.TAG.Format (toTAGDependency, toTAGTest, toTAGSentence, SpineExist, tagRoot, root) where 

--{{{  Imports 
import Helpers.Common
import Data.Array
--import NLP.Language hiding (mkNonTerm)
import NLP.WordLattice
import NLP.Grammar.TAG hiding (adjPos)
import NLP.Grammar.Spine

import NLP.Grammar.Dependency
import NLP.TreeBank.TreeBank
import qualified Data.Set as S
import qualified Data.Map as M
import Debug.Trace
import NLP.Model.TAG.Wrap
import NLP.Language.SimpleLanguage 
import NLP.ParseMonad
import Prelude hiding (mapM)
import Data.Traversable 
--}}}

tagRoot :: ParseMonad (TData, TSpine) 
tagRoot = do 
  aroot <- toAtom $ mkWord "ROOT"
  apos <- toAtom $ mkPOS "ROOT"

  let non = mkNonTerm "ROOT"
  anon <- toAtom non
  let spine =  mkSpine [anon]
  aspine <- toAtom $  mkSpine [non] 
  return ((GWord (aroot, apos), aspine) , spine)

root:: Int -> ParseMonad (TWord)
root ind = do 
    (word,spine) <- tagRoot
    return $ mkTAGWord  word spine ind 

toTAGSentence :: WordInfoSent  -> ParseMonad (Sentence (TWord))
toTAGSentence  (WordInfoSent wis)= do
  tr <- tagRoot
  return $ mkTAGWords $ [ ((GWord (word wi, pos'), aspine wi), tspine wi ) | 
                          wi <- elems wis,
                          pos' <- [head $ pos wi]
                        ] 
                        ++ [tr]

toTAGDependency :: WordInfoSent  -> ParseMonad TSentence 
toTAGDependency (WordInfoSent wis) = do
  (Sentence sent) <- toTAGSentence (WordInfoSent wis)
  return $ TAGSentence sent (depstruct sent)
    where 
          depstruct sent = Dependency $ M.fromList $ map (convertWI sent)  $ elems wis
          convertWI sent wi = (ind wi, 
                          if head == 0 then DEdge n $ AdjunctionInfo (adjPos wi) (sister wi) () 
                          else DEdge head $ AdjunctionInfo (adjPos wi) (sister wi) ())
              where n = latticeLength (Sentence sent)
                    head = adjoinInd wi

type SpineExist = M.Map (APOS) (S.Set (RSpine))

mkTAGWords words = 
    mkSentence $ newWords
        where newWords = map (\(i, (a,b)) -> mkTAGWord a b i) $ zip [1..] words

tagWordHelper :: AWord -> APOS -> Spine NonTerm -> Int -> ParseMonad TWord
tagWordHelper word pos sp ind = do
  aspine <- toAtom sp
  tspine <- mapM toAtom sp
  return $ mkTAGWord (GWord (word, pos), aspine) tspine ind 

mkTestTAGWord :: SpineExist  -> (Int, AWord, [APOS]) -> ParseMonad [TWord]
mkTestTAGWord counts  (ind, word, pos) =
     mapM (\(pos, sp) -> tagWordHelper word pos sp ind) spinels 
         where spinels = do
                 pos' <- pos
                 spine <- S.toList $ fromJustDef mempty $ 
                           M.lookup pos' counts
                 return $ (pos', spine)

toTAGTest :: SpineExist -> Bool -> WordInfoSent -> ParseMonad (SentenceLat TWord) 
toTAGTest counts useFirstPOS (WordInfoSent wis) = do 
  r <- root (n+1)
  testTagWords <- mapM (mkTestTAGWord counts ) $ 
                  [ (ind wi, word wi, if useFirstPOS then [head $ pos wi] else pos wi) | 
                    wi <- elems wis] 
  return $ mkSentenceLat $ testTagWords  ++ [[r]]
      where
            (_,n) = bounds wis


-- writeSpineMap file = do
--   spine <- decodeFile file :: (IO (SpineExist)) 
--   let mapping = zip [1..] $ S.toList $ S.unions $ map snd $ M.toList spine
--   return $ render $ vcat $ map (\(a,b)-> (int a)<+>(text$ show b)) mapping

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
