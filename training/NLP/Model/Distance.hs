{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, FlexibleContexts #-}
module NLP.Model.Distance (mkDistCacheLeft, mkDistCacheRight, testDist,
                          Delta, VerbDistance, prevComma, adjacent, withPrevComma, resetDelta, startDelta ,
                         DisCache, SurfaceFeature(..),
                                 mkVerbDis
                          ) where 

--{{{  Imports 
import Helpers.Common
import qualified Data.Bimap as BM
import NLP.WordLattice
import NLP.Language.SimpleLanguage
import NLP.ParseMonad
import qualified Data.Map as M
import NLP.ChartParse
import Debug.Trace
import Helpers.Test

--}}}
data SurfaceFeature = SurfaceFeature {
      containsVerb :: VerbDistance,
      endsWithComma :: Bool
    } deriving (Eq, Ord, Show)

type DisCache = (Int,Int) -> SurfaceFeature

distCacheRight :: (Int -> Bool) -> (Int -> Bool) -> (Int) -> (Int, Int) ->  SurfaceFeature
distCacheRight isVerb isCommaOrConj n (i,k) = 
    SurfaceFeature { containsVerb = VerbDistance $ any isVerb [i+1..k],
                     endsWithComma = (k + 1 == n)  || (k == n)   
                                     || (isCommaOrConj k) 
                                     || (isCommaOrConj (k+1))}

mkDistCacheRight :: (WordLattice sent, WordSymbol (Symbol sent)) => sent -> ParseMonad DisCache 
mkDistCacheRight sent = do
  verbFn <- isVerb
  commaFn <- isComma
  conjFn <- isConj
  let get = getPOS . head . getWords sent
  let cache = m (verbFn.get) (commaFn.get) (conjFn .get) 
  return $  \i -> fromJustNote "dist cache right" $ M.lookup i cache 
    where m vfn cfn ccfn = M.fromList $ do
                i <- [1..n] 
                k <- [i..n]
                return $ ((i,k), distCacheRight vfn (\a -> cfn a || ccfn a) n (i,k) )
          n = latticeLength sent


distCacheLeft  :: (Int -> Bool) -> (Int -> Bool) -> (Int) -> (Int, Int)  -> SurfaceFeature
distCacheLeft isVerb isCommaOrConj n(i,k) = 
    SurfaceFeature (VerbDistance $ any isVerb [i-1, i-2..k])
                   True


mkDistCacheLeft :: (WordLattice sent,  WordSymbol (Symbol sent)) => sent -> ParseMonad DisCache 
mkDistCacheLeft sent = do 
  verbFn <- isVerb
  commaFn <- isComma
  let get = getPOS . head . getWords sent
  let cache = m (verbFn.get) (commaFn.get) 
  return $ \i -> fromJustNote "left cache" $ M.lookup i cache
    where m vfn cfn = M.fromList $ do
                i <- [n,n-1..1] 
                k <- [i, i-1..1]
                let dis = distCacheLeft vfn cfn n (i,k)
                return $ ((i,k), dis)
          n = latticeLength sent

newtype VerbDistance = VerbDistance { spansVerb  :: Bool -- Is there a verb in the surface string between
                                    } 
    deriving (Show, Eq, Ord, Enum, Bounded, Binary, Arbitrary)

newtype Delta = Delta (Bool,Bool,Bool)    
   deriving (Eq, Ord, Bounded, Binary, Arbitrary)

withPrevComma (Delta (_,b,c) ) = Delta (True, b, c) 

prevComma (Delta (a,_,_)) = a
prevConj (Delta (_,b,_)) = b
adjacent (Delta (_,_,c)) = c

instance Show Delta where 
    show (Delta a) = show a

deltaMap = BM.fromList $ zip [0..] [
            Delta (a, b, c) | 
            a <- [False, True],
            b <- [False, True],
            c <- [False, True]
           ]

startDelta = Delta (False, False, True)

instance Enum Delta where
    {-# INLINE fromEnum #-} 
    fromEnum (Delta (False, False, False)) = 0 
    fromEnum (Delta (False, False, True))  = 1
    fromEnum (Delta (False, True, False))  = 2
    fromEnum (Delta (False, True, True) )  = 3
    fromEnum (Delta (True,  False, False)) = 4 
    fromEnum (Delta (True,  False, True))  = 5 
    fromEnum (Delta (True,  True, False))  = 6
    fromEnum (Delta (True,  True, True))   = 7 
    toEnum n =  fromJustNote "delta" $ BM.lookup n deltaMap

instance Pretty Delta where 
    pPrint = text . show 

resetDelta = Delta (False, False, False)

instance Pretty VerbDistance where 
    pPrint d = text $ showBool (spansVerb d)
        where
          showBool True = "1"
          showBool False = "0"

mkVerbDis = VerbDistance 

--{{{  TESTS


testDist = testGroup "Dist props" [
           testProperty "start delta" prop_startDelta,
           testProperty "reset delta" prop_resetDelta,
           testProperty "comma delta" prop_prevComma,
           testProperty "enum delta" prop_enumDelta,
           testCase "right test" test_distCacheRight,
           testProperty "left test" prop_distCacheLeft
        ]


prop_startDelta = (adjacent $ startDelta) == True
prop_resetDelta = (adjacent $ resetDelta) == False
prop_prevComma d = (prevComma $ withPrevComma d) == True

prop_enumDelta d = (toEnum $ fromEnum d) == d
    where types = (d :: Delta)

prop_distCacheLeft ls = do
  i <- choose (1,n)
  k <- choose (1,i)
  return $ (distCacheLeft isVerb isComma n (i,k) == SurfaceFeature (VerbDistance $ any isVerb [i-1,i-2..k]) True)    
     where types = (ls::[Bool])
           n = length ls 
           isVerb = (\a -> ls !! a) 
           isComma = (\a -> ls !! a) 


data VerbComma = V | C | N
                 deriving (Eq, Ord, Show)

simpleSentence = [N, N, N, C, N, N, V, C, N]

test_distCacheRight = do 
  let verbs = [("no verb", False, (1,3)), 
               ("no verb", False, (5,6)), 
               ("no verb", False, (7,8)), 
               ("yes verb", True,  (5,7))] 
  mapM_ (\(a,b,c) -> assertEqual a (VerbDistance b) (containsVerb $ dcr c)) verbs
  let commas = [("yes comma", True, (1,3)),
                ("no comma", False, (1,5))
              ]
  mapM_ (\(a,b,c) -> assertEqual a b (endsWithComma $ dcr c)) commas
  where
    dcr = distCacheRight isVerb isComma (length simpleSentence)
    sent = M.fromList $ zip [1..] simpleSentence
    isVerb i = (== V) $ (M.!)  sent i 
    isComma i = (== C) $ (M.!)  sent i
--}}}
