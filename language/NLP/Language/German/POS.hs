{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
module NLP.Language.German.POS  where 
import NLP.Language.Common
import qualified NLP.Language.English.POS as J
import qualified Data.Bimap as BM
 
posMap = BM.fromList [(DOLCOM, "$,"),
                      (DOLDOT, "$."),
                      (DOLPAR, "$(")
                     ]

data POSCore = ADJA | ADJD | ADV | APPR | APPRART | APPO | APZR | ART | CARD | FM | ITJ | KOUI | KOUS | KON | KOKOM | NN | NE | PDS | PDAT | PIS | PIAT | PIDAT | PPER | PPOS | PPOSAT | PRELS | PRELAT | PRF | PWS | PWAT  | PWAV | PAC | PTKZU | PTKNEG | PTKVZ | PTKANT | PTKA | SGML | SPELL | TRUNC | VVFIN | VVIMP | VVINF | VVIZU | VVPP | VAFIN | VAIMP | VAINF | VAPP | VMFIN | VMINF | VMPP | XY | DOLCOM | DOLDOT | DOLPAR
            deriving (Eq, Ord, Enum, Bounded, Show, Read)

$( derive makeBinary ''POSCore )
$( derive makeArbitrary ''POSCore )

newtype POS = POSWrap POSCore 
    deriving (Eq, Ord, Enum, Bounded, Binary, Arbitrary, Read)
             
instance Show POS where 
    show (POSWrap pcore) = fromMaybe (show pcore) $ BM.lookup pcore posMap

instance Pretty POS where pPrint = text . show  

mkPOS :: String -> POS
mkPOS str = POSWrap $ fromMaybe ( readNote ("POS " ++ str) str) $ 
            BM.lookupR str posMap


toJoint :: POS ->  J.POS
toJoint (POSWrap pos) = J.POSWrap $ 
    case pos of 
      ADJA -> J.JJ
      ADJD -> J.JJ
      ADV -> J.RB 
      APPR -> J.IN
      APPRART -> J.IN 
      APPO -> J.IN
      APZR -> J.IN
      ART -> J.DT
      CARD -> J.CD
      FM -> J.FW
      ITJ -> J.UH
      -- These ones are tricky
      KOUI -> J.RB
      KOUS -> J.IN
      KON -> J.CC
      KOKOM -> J.IN
      NN -> J.NN
      NE -> J.NNP
      PDS -> J.DT
      PDAT -> J.DT 
      PIS -> J.DT 
      PIAT -> J.DT 
      PIDAT -> J.DT
      PPER -> J.PRP
      PPOS -> J.PRPDOL
      PPOSAT -> J.PRPDOL
      PRELAT -> J.DT
      PRF -> J.PRP 
      PWS -> J.WP
      PWAT -> J.WP
      PWAV -> J.WRB
      PTKZU -> J.TO
      PTKNEG -> J.RB
      PTKVZ -> J.RB
      PTKANT -> J.RB
      PTKA -> J.RB
      SGML -> J.SYM
      SPELL -> J.SYM
      TRUNC -> J.SYM
      VVFIN -> J.VB
      VVIMP -> J.VB
      VVINF -> J.VB
      VVIZU -> J.VB
      VVPP -> J.VBN
      VAFIN -> J.VB       
      VAIMP -> J.VB       
      VAINF -> J.VB            
      VAPP -> J.VBN       
      VMFIN -> J.MD            
      VMINF -> J.MD         
      VMPP -> J.MD      
      XY -> J.SYM
      DOLCOM -> J.COM
      DOLDOT -> J.DOT
      DOLPAR -> J.DOT




