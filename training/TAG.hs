{-# LANGUAGE TypeSynonymInstances, TypeFamilies, FlexibleInstances, DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module TAG where 
import Control.Monad (liftM, ap)
import qualified Sentence as S
import Sentence  
import DependencyStructure
import Test.QuickCheck
import ArbitraryHelpers
import Data.Char (toUpper)
import Data.List
import Data.Function (on)
import Data.Maybe (isNothing, fromMaybe, catMaybes)
import Safe (fromJustNote, atNote)
import NonTerm
import POS 
import Debug.Trace
import Debug.Trace.Helpers
import Control.Exception
import Text.PrettyPrint.HughesPJClass
import Test.HUnit hiding (State, Node, assert)
import NonTerm
import Data.DeriveTH hiding (Derivation)
import Data.Binary
--import StringTable.Atom
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BS
import POS 
-- | A non-terminal in the TAG grammar 

data ParseTree = 
    Node NonTerm [ParseTree] | 
    Leaf GWord 


niceParseTree (Leaf (w,pos)) = 
                (text $ show pos) <+> (text $ show w) 
niceParseTree (Node nt rest) =  
    hang (text $ show nt) 3 (vcat $ map niceParseTree rest)


instance Show ParseTree where 
    show = render.pPrint

instance Pretty ParseTree where 
    pPrint (Leaf (w,pos)) = 
       lparen <> (text $ show pos) <+> (text $ show w) <> rparen 
    pPrint (Node nt rest) =  
        
        (lparen <> (text $ show nt))
        <+>
        (hsep $ map pPrint rest) <> rparen




  


convertToTree tagsent = head n  
    where
      (Node _ n) = convertNewTree $ root rootPos
      rootPos = (slength $ tsSent tagsent) + 1
      flat = flattenDep $ tsDep tagsent
      sent = tsSent tagsent
      convertNewTree tw = 
              convertToTree' tw ((lastOfSpine $ twSpine tw) -1)
      convertToTree' tw (-1) = Leaf (twWord tw) 
      convertToTree' tw spos = 
              Node (getNonTerm spos $ twSpine tw) $ 
                   (reverse $ map convertNewTree $ atSpos left) ++ 
                   ([convertToTree' tw (spos-1)]) ++ 
                   (map convertNewTree $ atSpos right)
                  where  (_, (left, right)) = flat !! ((twInd tw) -1)
                         atSpos = map (\ainfo -> (getWord sent $ adjPos ainfo)). catMaybes.  
                                  fromMaybe [] . lookup spos . alignWithSpine (twSpine tw)




mkTagWords words = 
    mkSentence $ newWords
               where newWords = map (\(i, (a,b)) -> mkTAGWord a b i) $ zip [1..] words


rootNT = mkNonTerm "ROOT"
instance WordSym TAGWord where
    root i = mkTAGWord (root i) (mkSpine [rootNT]) i
    isRoot = isRoot . twWord 
instance Arbitrary TAGSentence where 
    arbitrary = do
      sent <- listOf1 arbitrary
      let tsent = mkTagWords sent 
      dep <- arbDepMap (slength tsent) (pickAdjInd tsent)
      return $ TAGSentence tsent dep
          where pickAdjInd sent i = do 
                  let tagWord = getWord sent i
                  let (Spine sp _) = twSpine tagWord
                  adjPos <- choose (0,length sp -1)
                  return $ AdjunctionInfo adjPos Sister ()


data AdjunctionInfo a  = 
    AdjunctionInfo {adjPos :: Int, -- adjunction position
                    adjType :: AdjunctionType, -- sister adjunction? 
                    adjInfo :: a } 
    deriving (Show, Read, Eq)



-- | Takes a spine and an ordered list of adjunctions, 
--   returns the list of adjunctions with epsilons inserted 
alignWithSpine :: Spine -> [DEdge (AdjunctionInfo a)] -> [(Int, [Maybe (AdjunctionInfo a)])] 
alignWithSpine (Spine spine _) adjs =
    [(pos, getAdj nt pos ++ [Nothing]) | 
     (nt, pos) <- zip spine [0..]] 
    where 
      indexedAdj = M.fromListWith (flip (++)) $ 
                   map (\(DEdge to ainfo) -> (adjPos ainfo, [ainfo {adjPos = to}]) ) adjs
      getAdj nt ind  = 
          case M.lookup ind indexedAdj of
            Nothing -> [] 
            Just adjs -> map Just adjs  



testAlign = TestCase $  
            mapM_ (\ ((sp, adj), ans) -> assertEqual "alignment" ans (alignWithSpine sp adj)) test
    where  test = [((mkSpine [mkNonTerm "A", mkNonTerm "B"], [DEdge 10 $ AdjunctionInfo 1 Regular () ]), 
                    [(0,[Nothing]),(1, [Just $ AdjunctionInfo 10 Regular (),Nothing])]),

                   ((mkSpine [mkNonTerm "A", mkNonTerm "B"], [DEdge 10 $ AdjunctionInfo 1 Regular () , 
                                                            DEdge 12 $ AdjunctionInfo 1 Regular () , 
                                                            DEdge 15 $ AdjunctionInfo 1 Sister ()   ]), 
                    [(0,[Nothing]),(1,[Just $ AdjunctionInfo 10 Regular () ,
                                       Just $ AdjunctionInfo 12 Regular (), 
                                       Just $ AdjunctionInfo 15 Sister (), Nothing])]),
                  ((mkSpine [mkNonTerm "A"], [DEdge 4 $ AdjunctionInfo 0 Sister (), 
                                            DEdge 3 $ AdjunctionInfo 0 Sister (), 
                                            DEdge 2 $ AdjunctionInfo 0 Sister ()]), 
                    [(0,[Just $ AdjunctionInfo 4 Sister (), 
                         Just $ AdjunctionInfo 3 Sister (), 
                         Just $ AdjunctionInfo 2 Sister (), Nothing])])]

isVerb gword = isPOSVerb pos
    where (_, pos) = gword 





-- prop_directCheck tagsent = --trace ((show finalSemi) ++ "\n\n" ++ (show $ directCounts tagsent))  $  
--     (directCounts tagsent == fromDerivation finalSemi)
--     where types = tagsent:: (TAGSentence (Derivation TAGTrainingCounts)) 
--           (TAGSentence sent _) = tagsent 
--           fsms = tagSentenceFSMs tagsent mkTagDepDerivation
--           getFSM i word = fsms !! (i-1)
--           (Just finalSemi, _) = eisnerParse getFSM sent
