{-# LANGUAGE TemplateHaskell #-}

module NLP.Grammar.TAG where 

--{{{ Imports
import NLP.Grammar.Dependency
import Helpers.Common
import NLP.Grammar.Spine as S
import qualified Data.Map as M
import Data.Array
import Helpers.Test
--}}}

--{{{  AdjunctionType
-- | There are two types of adjunction, 
--   Sister comes from a single position, 
--   regular duplicates the node in the head tree 
data AdjunctionType = Sister | Regular
    deriving  (Eq, Ord, Enum, Bounded)

--{{{AdjunctionType Classes 

instance Pretty AdjunctionType where 
    pPrint Sister = text "SIS" 
    pPrint Regular = text "REG" 


instance Show AdjunctionType where 
    show Sister = "s"
    show Regular = "a"

$( derive makeBinary ''AdjunctionType ) 
$( derive makeArbitrary ''AdjunctionType ) 
$( derive makeNFData ''AdjunctionType)
--}}} 
--}}}

--{{{  AdjunctionInfo
-- | Adjunction info describes the adjunction between two trees (spines). 
--   For convenience it also is a functor to carry extra information

data AdjunctionInfo a  = 
    AdjunctionInfo {adjPos :: Int, -- adjunction position
                    adjType :: AdjunctionType, -- sister adjunction? 
                    adjInfo :: a } 
    deriving (Show, Eq, Ord)

--{{{AdjunctionInfo Classes
instance Functor AdjunctionInfo where 
    fmap f a = a {adjInfo = f $ adjInfo a}
--}}}

--}}}

--{{{  TAGSentence
-- | A TAG sentence is our internal representation of a TAG Tree. 
-- tsSent is the actual sentence (including spines for each word) 
-- twDep is the dependency structure over the spines 
data TAGSentence nt word = 
    TAGSentence { tsSent :: Array Int (TAGWord nt word),
                  tsDep  :: Dependency (AdjunctionInfo ())}
    deriving Show

--{{{ TAGSentence Classes 


--}}} 
--}}}


--{{{  TAGWord
-- | A TAGWord is an individual word located in a sentence and paired with a spine. 
--   There can be many words with the same index as long as they have different spines
data TAGWord nt twdata = TAGWord {
      twSpine :: Spine nt,
      twData  :: twdata,
      twInd   :: Int 
} deriving (Eq,Ord)


mkTAGWord :: w -> Spine nt -> Int -> TAGWord nt w
mkTAGWord w s ind = TAGWord s w ind

--{{{ TAGWord Classes

instance (Show w, Show nt) => Show (TAGWord nt w) where 
    show = render . pPrint

instance (Show w, Show nt) => Pretty (TAGWord nt w) where 
    pPrint (TAGWord word spine ind) = (text $ show ind)  <+> (text " ") <+> (text $ show word) <+> (text $ show spine) 

--}}} 
--}}}

-- | Predicate - does the adjunction of two tagword at a certain pos agree with the TAG sentence
valid :: (TAGSentence nt w) -> TAGWord nt w -> Maybe (TAGWord nt w) -> Int -> AdjunctionType -> Bool
valid _ _ Nothing _ _ = True
valid (TAGSentence sent dep) head (Just child) pos atype =
    case getHead dep $ twInd child of 
      Nothing -> False  -- (not $ isRoot child) && 
      Just (DEdge h (AdjunctionInfo pos' atype' _)) -> 
          (h == twInd head) && (pos == pos') && (atype == atype')


-- | Takes a spine and an ordered list of adjunctions, 
--   returns the list of adjunctions with epsilons inserted 
alignWithSpine :: Spine nt -> [DEdge (AdjunctionInfo a)] -> [(Int, [Maybe (AdjunctionInfo a)])] 
alignWithSpine sp adjs =
    [(pos, getAdj nt pos ++ [Nothing]) | 
     (nt, pos) <- zip spine [0..]] 
    where 
      indexedAdj = M.fromListWith (flip (++)) $ 
                   map (\(DEdge to ainfo) -> (adjPos ainfo, [ainfo {adjPos = to}]) ) adjs
      getAdj nt ind  = 
          case M.lookup ind indexedAdj of
            Nothing -> [] 
            Just adjs -> map Just adjs  
      spine = S.toList sp

--{{{  TESTS


instance (Arbitrary a, Arbitrary b) => Arbitrary (TAGWord a b) where 
    arbitrary = do 
      ind <- choose (1,20)
      spine <- arbitrary
      info <- arbitrary
      return $ mkTAGWord info spine ind

runTests = defaultMain [testTAG]

testTAG = testGroup "TAG props" [
           testProperty "tag word" prop_mkTAGWord
        ]

prop_mkTAGWord tw = tw == mkTAGWord (twData tw) (twSpine tw) (twInd tw) 
    where types = tw :: TAGWord Int Char

--}}}
