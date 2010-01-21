{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module NLP.Model.Distance where 

--{{{  Imports 
import Helpers.Common
import qualified Data.Bimap as BM
import NLP.Grammar.TAG
import NLP.WordLattice
import NLP.Language
import qualified Data.Map as M
import NLP.ChartParse
import NLP.Model.TAGWrap
--}}}

type DisCache = (Int,Int) -> (Bool,Bool) 

mkDistCacheRight :: (Language l, WordLattice sent, Symbol sent ~ (TWord l)) => sent -> DisCache 
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
          n = latticeLength sent

mkDistCacheLeft :: (Language l, WordLattice sent, Symbol sent ~ (TWord l)) => sent -> DisCache 
mkDistCacheLeft sent = \i -> case i of
                                (_, k ) | k == n +1 -> (False,True)
                                _ -> fromJustDef (False, False) $ M.lookup i m
    where m = M.fromList $ do
                i <- [n,n-1..1] 
                k <- [i, i-1..1]
                let dis = (or [twIsVerb $ head $ getWords sent j | j<-[i-1, i-2..k]] ,
                           True)
                return $ ((i,k), dis)
          n = latticeLength sent

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
    {-# INLINE fromEnum #-} 
    fromEnum (Delta False False False) = 0 
    fromEnum (Delta False False True)  = 1
    fromEnum (Delta False True False)  = 2
    fromEnum (Delta False True True)   = 3
    fromEnum (Delta True  False False) = 4 
    fromEnum (Delta True  False True)  = 5 
    fromEnum (Delta True  True False)  = 6
    fromEnum (Delta True  True True)   = 7 
    toEnum n =  fromJustNote "delta" $ BM.lookup n deltaMap

instance Pretty Delta where 
    pPrint = text . show 

resetDelta = Delta False False False

$( derive makeBinary ''VerbDistance )
$( derive makeBinary ''Delta )
$( derive makeArbitrary ''Delta )

instance Pretty VerbDistance where 
    pPrint d = text $ showBool (spansVerb d)
        where
          showBool True = "1"
          showBool False = "0"
