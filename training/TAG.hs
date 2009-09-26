{-# LANGUAGE TypeSynonymInstances, TypeFamilies, FlexibleInstances, DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module TAG where 
import Control.Monad (liftM, ap)
import qualified Sentence as S
import Sentence hiding (Word) 
import DependencyStructure
import Test.QuickCheck
import ArbitraryHelpers
import Data.Char (toUpper)
import Data.List
import Data.Function (on)
import Data.Maybe (isNothing, fromMaybe, catMaybes)
import Safe (fromJustNote, atNote)

import Debug.Trace
import Debug.Trace.Helpers
import Control.Exception
import Text.PrettyPrint.HughesPJClass
import Test.HUnit hiding (State, Node, assert)

import Data.DeriveTH hiding (Derivation)
import Data.Binary
--import StringTable.Atom
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BS

-- | A non-terminal in the TAG grammar 
newtype NonTerm = NonTerm BS.ByteString 
    deriving (Eq, Ord, Binary)

--instance Binary NonTerm where 
--    put (NonTerm p) = 
--      put $ ((fromAtom p) ::String)
--    get = do 
--      w <- get
--      return $ mkNonTerm (w::String)

mkNonTerm = NonTerm . BS.pack

instance Show NonTerm where 
    show (NonTerm nt) = BS.unpack nt

instance Arbitrary NonTerm where
    arbitrary = 
      mkNonTerm `liftM` map toUpper `liftM` listOf1 (elements alpha) 


data ParseTree = 
    Node NonTerm [ParseTree] | 
    Leaf GWord 



instance Show ParseTree where 
    show = render.pPrint


instance Pretty ParseTree where 
    pPrint (Leaf (w,pos)) = 
       lparen <> (text $ show pos) <+> (text $ show w) <> rparen 
    pPrint (Node nt rest) =  
        
        (lparen <> (text $ show nt))
        <+>
        (hsep $ map pPrint rest) <> rparen
-- | There are two types of adjunction, 
--   Sister comes from a single position, 
--   regular duplicates the node in the head tree 
data AdjunctionType = Sister | Regular
    deriving  (Eq, Ord, Enum)

data AdjunctionSide = ALeft | ARight
                    deriving (Eq, Ord, Enum, Show)

instance Show AdjunctionType where 
    show Sister = "s"
    show Regular = "a"

instance Arbitrary AdjunctionType where 
    arbitrary = elements [Sister, Regular]

$( derive makeBinary ''AdjunctionType ) 

  
newtype Spine = Spine [NonTerm]
    deriving (Eq, Ord, Binary)

top (Spine []) = Nothing
top (Spine nts) = Just $ last nts

lastOfSpine (Spine nts) = length nts

getNonTerm i (Spine nts) = atNote "getNonTerm" nts i

fromPOS (POS ps) = NonTerm ps 

lookupNonTerm i (Spine nts) =
    if i >= length nts || i < 0 then Nothing
    else Just $ nts !! i

instance Show Spine where 
    show (Spine nts) = intercalate "+" $ ["*"] ++ map show nts


data TAGWord = TAGWord {
      twInd   :: Int,
      twWord  :: GWord,
      twSpine :: Spine}
               deriving (Eq, Ord, Show)

instance Pretty TAGWord where 
    pPrint (TAGWord ind word spine) = (text $ show ind)  <+> (text " ") <+> (text $ show word) <+> (text $ show spine) 

-- instance Context GWord where 
--     type Sub (GWord) = String
--     decompose (S.Word word, POS pos) = [pos, word] 
--     compose [pos, word] = (S.Word word, POS pos)


data TAGSentence  = 
    TAGSentence { tsSent :: Sentence TAGWord,
                  tsDep  :: Dependency (AdjunctionInfo ())}
                deriving (Show)

convertToTree tagsent = head n  
    where
      (Node _ n) = convertNewTree (root ((slength $ tsSent tagsent) + 1))
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
                         atSpos = map (\ainfo -> getWord sent $ adjPos ainfo). catMaybes.  
                                  fromMaybe [] . lookup spos . alignWithSpine (twSpine tw)
mkTagWords words = 
    mkSentence $ newWords
               where newWords = map (\(i, (a,b)) -> TAGWord i a b) $ zip [1..] words

instance Arbitrary Spine where
    arbitrary = Spine `liftM` listOf1 arbitrary

rootNT = mkNonTerm "ROOT"
instance WordSym TAGWord where
    root ind = TAGWord ind (root ind) (Spine [rootNT])


instance Arbitrary TAGSentence where 
    arbitrary = do
      sent <- listOf1 arbitrary
      let tsent = mkTagWords sent 
      dep <- arbDepMap (slength tsent) (pickAdjInd tsent)
      return $ TAGSentence tsent dep
          where pickAdjInd sent i = do 
                  let tagWord = getWord sent i
                  let Spine sp = twSpine tagWord
                  adjPos <- choose (0,length sp -1)
                  return $ AdjunctionInfo adjPos Sister ()


data AdjunctionInfo a  = 
    AdjunctionInfo {adjPos :: Int, -- adjunction position
                    adjType :: AdjunctionType,
                    adjInfo :: a } -- sister adjunction? 
    deriving (Show, Eq)



-- | Takes a spine and an ordered list of adjunctions, 
--   returns the list of adjunctions with epsilons inserted 
alignWithSpine :: Spine -> [DEdge (AdjunctionInfo a)] -> [(Int, [Maybe (AdjunctionInfo a)])] 
alignWithSpine (Spine spine) adjs =
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
    where  test = [((Spine [mkNonTerm "A", mkNonTerm "B"], [DEdge 10 $ AdjunctionInfo 1 Regular () ]), 
                    [(0,[Nothing]),(1, [Just $ AdjunctionInfo 10 Regular (),Nothing])]),

                   ((Spine [mkNonTerm "A", mkNonTerm "B"], [DEdge 10 $ AdjunctionInfo 1 Regular () , 
                                                            DEdge 12 $ AdjunctionInfo 1 Regular () , 
                                                            DEdge 15 $ AdjunctionInfo 1 Sister ()   ]), 
                    [(0,[Nothing]),(1,[Just $ AdjunctionInfo 10 Regular () ,
                                       Just $ AdjunctionInfo 12 Regular (), 
                                       Just $ AdjunctionInfo 15 Sister (), Nothing])]),
                  ((Spine [mkNonTerm "A"], [DEdge 4 $ AdjunctionInfo 0 Sister (), 
                                            DEdge 3 $ AdjunctionInfo 0 Sister (), 
                                            DEdge 2 $ AdjunctionInfo 0 Sister ()]), 
                    [(0,[Just $ AdjunctionInfo 4 Sister (), 
                         Just $ AdjunctionInfo 3 Sister (), 
                         Just $ AdjunctionInfo 2 Sister (), Nothing])])]

isVerb tword = (take 2 $ BS.unpack pos) == "VB"
    where (_, POS pos) = twWord tword 





-- prop_directCheck tagsent = --trace ((show finalSemi) ++ "\n\n" ++ (show $ directCounts tagsent))  $  
--     (directCounts tagsent == fromDerivation finalSemi)
--     where types = tagsent:: (TAGSentence (Derivation TAGTrainingCounts)) 
--           (TAGSentence sent _) = tagsent 
--           fsms = tagSentenceFSMs tagsent mkTagDepDerivation
--           getFSM i word = fsms !! (i-1)
--           (Just finalSemi, _) = eisnerParse getFSM sent
