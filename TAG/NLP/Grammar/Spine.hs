{-# LANGUAGE TemplateHaskell, Rank2Types, ScopedTypeVariables #-}
module NLP.Grammar.Spine where 
--import NLP.Grammar.NonTerm 
import Helpers.MkEnum
import Helpers.Common
--import NLP.Language

type Spine nt = [nt] 

mkSpine nts =  nts 

--{{{  Spine Classes
-- instance (Language l) => Show (Spine l) where 
--     show ( nts) = intercalate "+" $ ["*"] ++ map show nts

-- instance (Language l) => Enum (Spine l) where 
--     fromEnum (Spine s i) = i  
--     toEnum n = Spine (mkToEnumList maxBound n) n

-- -- WARNING, assumes that spines are size <= 4
-- instance (Language l) => Bounded (Spine l) where 
--     minBound = toEnum 0 
--     maxBound = toEnum $ computeMaxBound $ map fromEnum (replicate 4 (maxBound ::NonTermWrap l))

-- instance Eq (Spine l)  where 
--     (==) (Spine _ i) (Spine _ i') = i == i' 

-- instance Ord (Spine l)where 
--     compare (Spine _ i) (Spine _ i') = compare i i' 

-- instance (Language l) => Pretty (Spine l) where 
--     pPrint (sp)  = text $ intercalate "->" $ map show $ reverse sp 
--}}}

-- Returns the non-terminal at the top of the spine (nothing if it has an empty spine)
top :: Spine nt -> Maybe (nt)
top  [] = Nothing
top  nts = Just $ last nts

-- Return the top position of the spine 
lastOfSpine :: Spine nt -> Int
lastOfSpine nts = length nts

getNonTerm :: Int -> Spine nt -> nt
getNonTerm i  nts = atNote "getNonTerm" nts i

lookupNonTerm :: Int -> Spine nt -> Maybe nt
lookupNonTerm i nts =
    if i >= length nts || i < 0 then Nothing
    else Just $ nts !! i


--{{{  TESTS


-- instance (Language l) => Arbitrary (Spine l) where
--     arbitrary = do
--       i <- choose (0,3)
--       nts <- vectorOf i arbitrary 
--       return $ mkSpine nts

--prop_enumSpine a = checkEnum
--    where types = (a::Spine ) 
--}}}

-- hasNP (Spine spine _) = any ((==) (NonTermWrap NP)) spine
-- hasNPorNPCC (Spine spine _) = any (\nt -> (nt == (NonTermWrap NP)) || (nt == (NonTermWrap NP_CC))) spine
-- posNP (Spine spine _) = fromJustNote "posnp" $ elemIndex (NonTermWrap NP) spine

-- addNPB s@(Spine spine _) = mkSpine (take pos spine ++ [NonTermWrap NPB] ++ drop pos spine) 
--    where pos = posNP s

-- isNPB (NonTermWrap NPB) = True
-- isNPB _ = False