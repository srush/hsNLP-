{-# LANGUAGE TemplateHaskell, Rank2Types, ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
module NLP.Grammar.Spine (mkSpine, Spine, top, lastOfSpine, getNonTerm, lookupNonTerm, parseSpine, toList, testSpine) where 

--{{{  Imports

import Helpers.Common hiding (char)
import Helpers.Parse
import Helpers.Test
import Control.Applicative
import Data.Traversable
import Data.Foldable
--}}}

newtype Spine nt = Spine [nt] 
    deriving (Eq, Ord, Binary, NFData, Functor, Foldable, Traversable, Show, Read)
mkSpine nts = Spine nts 

--{{{  Spine Classes

--instance (Show n) => Show (Spine n) where 
--    show (Spine nts) = intercalate "+" $ ["*"] ++ map show nts

instance (Pretty l) => Pretty (Spine l) where 
     pPrint (Spine sp)  = hcat $ punctuate (text "+") $ [text "*"] ++ map pPrint sp
--}}}

-- Returns the non-terminal at the top of the spine (nothing if it has an empty spine)
top :: Spine nt -> Maybe (nt)
top  (Spine []) = Nothing
top  (Spine nts) = Just $ last nts

-- Return the top position of the spine 
lastOfSpine :: Spine nt -> Int
lastOfSpine (Spine nts) = length nts

getNonTerm :: Int -> Spine nt -> nt
getNonTerm i (Spine nts) = atNote "getNonTerm" nts i

lookupNonTerm :: Int -> Spine nt -> Maybe nt
lookupNonTerm i (Spine nts) =
    if i >= length nts || i < 0 then Nothing
    else Just $ nts !! i

parseSpine :: Parser (nt) -> Parser (Spine (nt))
parseSpine parseNT = do 
      nonterms <- choice [Just `liftM` parseNT, 
                          char '*' >> return Nothing] 
                  `sepBy` char '+'
      return $ mkSpine $ catMaybes nonterms

--{{{  TESTS
runTests = defaultMain [testSpine]

instance (Arbitrary nt) =>  Arbitrary (Spine nt) where
     arbitrary = do
       i <- choose (0,3)
       nts <- vectorOf i arbitrary 
       return $ mkSpine nts

testSpine = testGroup "Spine props" [
         testProperty "top" prop_top,
         testProperty "last" prop_lastOfSpine,
         testProperty "mkSpine" prop_mkSpine
        ]

prop_top (sp::Spine Int) = 
    if null ls then top sp == Nothing
    else top sp == (Just $ last ls)
    where ls = toList sp

prop_lastOfSpine (sp::Spine Int) = 
    lastOfSpine sp == (length ls)
    where ls = toList sp

prop_mkSpine (sp::Spine Int) =
    sp == (mkSpine ls)
    where ls = toList sp

--}}}

