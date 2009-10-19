{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Distance where 

import Data.Monoid
import qualified Data.Bimap as BM
import Data.Binary
import Data.DeriveTH hiding (Derivation)
import TAG
import Sentence 
import Safe (fromJustNote, fromJustDef)
import qualified Data.Map as M
import Text.PrettyPrint.HughesPJClass
import Text.Printf
import NLP.ChartParse
import Debug.Trace
import Test.QuickCheck
type DisCache = (Int,Int) -> (Bool,Bool) 

mkDistCacheRight :: (SentenceLattice sent, Symbol sent ~ TAGWord) => sent -> DisCache 
mkDistCacheRight sent = \i -> case i of
                                (_, k ) | k == n +1 -> (False,True)
                                _ -> fromJustDef (False, False)  $ M.lookup i m 
    where m = M.fromList $ do
                i <- [1..n] 
                k <- [i..n]
                let dis = (or [twIsVerb $ head $ getWords sent j | j<-[i+1..k]],
                           (k+1 == n+1) 
                           || (twIsConj $ head $ getWords sent (k)) 
                           || (twIsConj $ head $ getWords sent (k+1)) 
                           || (twIsComma $ head $ getWords sent (k)) 
                           || (twIsComma $ head $ getWords sent (k+1)) )
                return $ ((i,k), dis)
          n = sentenceLength sent

mkDistCacheLeft :: (SentenceLattice sent, Symbol sent ~ TAGWord) => sent -> DisCache 
mkDistCacheLeft sent = \i -> case i of
                                (_, k ) | k == n +1 -> (False,True)
                                _ -> fromJustDef (False, False) $ M.lookup i m
    where m = M.fromList $ do
                i <- [n,n-1..1] 
                k <- [i, i-1..1]
                let dis = (or [twIsVerb $ head $ getWords sent j | j<-[i-1, i-2..k]] ,
                           True)
                return $ ((i,k), dis)
          n = sentenceLength sent


-- data Comma = NoComma | OneComma | TwoComma | ManyComma
--            deriving (Eq, Ord, Show, Enum, Bounded) 

-- toComma 0 = NoComma 
-- toComma 1 = OneComma 
-- toComma 2 = TwoComma 
-- toComma _ = ManyComma 

-- instance Monoid Comma where 
--     mempty = NoComma 
--     mappend a b = toComma (fromEnum a + fromEnum b)  

newtype VerbDistance = VerbDistance { spansVerb  :: Bool -- Is there a verb in the surface string between
                         } 
    deriving (Show, Eq, Ord, Enum, Bounded)

$( derive makeArbitrary ''VerbDistance )

data Delta = Delta {
      prevComma :: Bool,
      prevConj :: Bool,
      adjacent :: Bool
    }  
   deriving (Eq, Ord, Bounded)
 
instance Show Delta where 
    show (Delta a b c) = show (a,b,c)

deltaMap = BM.fromList $ zip [0..] [
            Delta a b c | 
            a <- [False, True],
            b <- [False, True],
            c <- [False, True]
           ]

startDelta = Delta {
               prevComma = False,
               prevConj = False,
               adjacent = True
             }

instance Enum Delta where 
    fromEnum d = fromJustNote "delta" $ BM.lookupR d deltaMap
    toEnum n =  fromJustNote "delta" $ BM.lookup n deltaMap

instance Pretty Delta where 
    pPrint = text . show 

--wordToDistance w = Distance {zeroLength = False,
--                             spansVerb  = twIsVerb w,
--                             numComma   = NoComma}

-- instance Monoid Distance  where 
--     mempty = Distance True False NoComma
--     mappend a b = Distance { zeroLength = zeroLength a && zeroLength b,
--                              spansVerb  = spansVerb a || spansVerb b,
--                              numComma = mappend (numComma a) (numComma b)
--                            } 


-- $( derive makeBinary ''Comma )
$( derive makeBinary ''VerbDistance )
$( derive makeBinary ''Delta )
$( derive makeArbitrary ''Delta )
instance Pretty VerbDistance where 
    pPrint d = text $ showBool (spansVerb d)
        where
          showBool True = "1"
          showBool False = "0"
