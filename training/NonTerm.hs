{-# LANGUAGE  GeneralizedNewtypeDeriving, TemplateHaskell #-}
module NonTerm (NonTerm, mkNonTerm, fromPOS, Spine(..), last, mkSpine, top, lastOfSpine, getNonTerm, lookupNonTerm)  where 
import POS 
import EnumHelpers
import Common

data PureNonTerm =  ADJP | ADJP_CC | ADVP | ADVP_CC | CONJP | FRAG | FRAG_CC | INTJ | INTJ_CC | LST | NAC | NAC_CC | NP | NP_CC | NX | NX_CC | PP | PP_CC | PRN | PRN_CC | PRT | PRT_CC | QP | QP_CC | RRC | RRC_CC | S | SBAR | SBARQ | SBARQ_CC | SBAR_CC | SINV | SINV_CC | SQ | SQ_CC | S_CC | UCP | UCP_CC | VP | VP_CC | WHADJP | WHADJP_CC | WHADVP | WHADVP_CC | WHNP | WHNP_CC | WHPP | X | X_CC | ROOT 
    deriving (Read, Show, Eq, Ord, Enum, Bounded)

data NonTerm = NonTermWrap PureNonTerm | NTPOS POS  
             deriving (Read, Eq, Ord)

instance Show NonTerm where
    show (NonTermWrap nt) = show nt
    show (NTPOS nt) = show nt

instance Pretty NonTerm where pPrint = text . show  

$( derive makeBinary ''PureNonTerm )
$( derive makeArbitrary ''PureNonTerm )

$( derive makeBinary ''NonTerm )
instance Arbitrary NonTerm where
    arbitrary = NonTermWrap `liftM` arbitrary


mkNonTerm :: String -> NonTerm
mkNonTerm nt = NonTermWrap $ readNote ("NonTerm" ++ nt) nt

fromPOS = NTPOS

instance Bounded NonTerm where
    minBound = NonTermWrap $ minBound
    maxBound = NTPOS $ maxBound 


instance Enum NonTerm where 
    fromEnum (NonTermWrap nt) = fromEnum nt
    fromEnum (NTPOS pos) = fromEnum (maxBound :: PureNonTerm) + 1 + fromEnum pos
    toEnum n = if n > fromEnum (maxBound :: PureNonTerm) then 
                     NTPOS $ toEnum (n - fromEnum (maxBound :: PureNonTerm) -1) 
                 else NonTermWrap $ toEnum (n ) 

prop_enumNonTerm a = checkEnum
    where types = (a::NonTerm) 


data Spine = Spine { nts   :: [NonTerm],
                     ienum :: Int} 
    
mkSpine nts = Spine nts ienum
    where ienum = mkFromEnumList nts maxBound  

instance Enum Spine where 
    fromEnum (Spine s i) = i  
    toEnum n = Spine (mkToEnumList maxBound n) n

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


lookupNonTerm i (Spine nts _) =
    if i >= length nts || i < 0 then Nothing
    else Just $ nts !! i

instance Show Spine where 
    show (Spine nts _) = intercalate "+" $ ["*"] ++ map show nts

instance Arbitrary Spine where 
    arbitrary = do 
      nts <- arbitrary 
      return $ mkSpine nts