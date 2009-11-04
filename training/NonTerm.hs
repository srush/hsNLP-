{-# LANGUAGE  GeneralizedNewtypeDeriving, TemplateHaskell #-}
module NonTerm (NonTerm, mkNonTerm, fromPOS, Spine(..), last, mkSpine, top, lastOfSpine, getNonTerm, lookupNonTerm, hasNP, posNP, addNPB, isNPB, hasNPorNPCC)  where 
import NLP.Language. 
import EnumHelpers
import Common
import Data.List (elemIndex)
import Test.QuickCheck

data NonTermWrap l = NonTermWrap PureNonTerm | NTPOS (POS l)  
             deriving (Read, Eq, Ord)

instance Show NonTermWrap where
    show (NonTermWrap nt) = show nt
    show (NTPOS nt) = show nt

instance Pretty NonTermWrap where pPrint = text . show  

$( derive makeBinary ''NonTermWrap )
instance Arbitrary NonTermWrap where
    arbitrary = NonTermWrap `liftM` arbitrary

mkNonTerm :: String -> NonTermWrap
mkNonTerm nt = NonTermWrap $ readNote ("NonTerm" ++ nt) nt

fromPOS = NTPOS

instance Bounded NonTermWrap where
    minBound = NonTermWrap $ minBound
    maxBound = NTPOS $ maxBound 

instance Enum NonTermWrap where 
    fromEnum (NonTermWrap nt) = fromEnum nt
    fromEnum (NTPOS pos) = fromEnum (maxBound :: PureNonTerm) + 1 + fromEnum pos
    toEnum n = if n > fromEnum (maxBound :: PureNonTerm) then 
                     NTPOS $ toEnum (n - fromEnum (maxBound :: PureNonTerm) -1) 
                 else NonTermWrap $ toEnum (n ) 

prop_enumNonTerm a = checkEnum
    where types = (a::NonTermWrap) 


data Spine = Spine { nts   :: [NonTermWrap],
                     ienum :: Int} 
    
mkSpine nts = Spine nts ienum
    where ienum = mkFromEnumList nts maxBound  

instance Enum Spine where 
    fromEnum (Spine s i) = i  
    toEnum n = Spine (mkToEnumList maxBound n) n

-- WARNING, assumes that spines are size <= 4
instance Bounded Spine where 
    minBound = toEnum 0 
    maxBound = toEnum $ computeMaxBound $ map fromEnum (replicate 4 (maxBound ::NonTermWrap))

instance Arbitrary Spine where
    arbitrary = do
      i <- choose (0,3)
      nts <- vectorOf i arbitrary 
      return $ mkSpine nts


instance Eq Spine where 
    (==) (Spine _ i) (Spine _ i') = i == i' 

instance Ord Spine where 
    compare (Spine _ i) (Spine _ i') = compare i i' 

$( derive makeBinary ''Spine )


instance Pretty Spine where 
    pPrint (Spine sp _)  = text $ intercalate "->" $ map show $ reverse sp 

prop_enumSpine a = checkEnum
    where types = (a::Spine) 


top (Spine [] _) = Nothing
top (Spine nts _) = Just $ last nts

lastOfSpine (Spine nts _) = length nts

getNonTerm i (Spine nts _) = atNote "getNonTerm" nts i


hasNP (Spine spine _) = any ((==) (NonTermWrap NP)) spine
hasNPorNPCC (Spine spine _) = any (\nt -> (nt == (NonTermWrap NP)) || (nt == (NonTermWrap NP_CC))) spine
posNP (Spine spine _) = fromJustNote "posnp" $ elemIndex (NonTermWrap NP) spine


addNPB s@(Spine spine _) = mkSpine (take pos spine ++ [NonTermWrap NPB] ++ drop pos spine) 
    where pos = posNP s

isNPB (NonTermWrap NPB) = True
isNPB _ = False

lookupNonTerm i (Spine nts _) =
    if i >= length nts || i < 0 then Nothing
    else Just $ nts !! i

instance Show Spine where 
    show (Spine nts _) = intercalate "+" $ ["*"] ++ map show nts

