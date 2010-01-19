{-# LANGUAGE  TemplateHaskell, GeneralizedNewtypeDeriving, TypeFamilies #-}
module NLP.CNF where 

--{{{  Imports

import Helpers.Common
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Bimap as BM
import NLP.Probability.ConditionalDistribution
import NLP.Probability.Distribution 
import NLP.Probability.Observation

--}}}

data NT = NT String | TOP | TermTOP
    deriving (Eq,Ord) 

--{{{  NT Classes
mkNT "*TOP*" = TOP 
mkNT "*TermTOP*" = TermTOP 
mkNT "**TOP**" = TOP 
mkNT "**TermTOP**" = TermTOP 
mkNT e = NT e 

termTop = TermTOP
top = TOP

instance Show NT where 
    show (NT n) = n  
    show TOP = "TOP"  
    show TermTOP = "**TERMTOP**"  
--}}}


newtype UnkWord = UnkWord String 
    deriving (Binary)

instance Eq UnkWord where 
     (==) a b = True 

instance Ord UnkWord where 
     compare a b = EQ 

data Word = KnownWord String | 
            UnknownWord UnkWord |
            EndWord
   deriving  (Eq, Ord)

--{{{  Word Classes
newtype WordTable = WordTable (M.Map String Int)
    deriving (Binary, Show)

mkWordTable words = WordTable $ 
  M.filter (> 5) $ 
  M.unionsWith (+) $ 
  [M.singleton word 1 | word <- words]
   
mkWord _ "*END*" = EndWord 
mkWord (WordTable m) w = 
    case M.lookup w m of 
      Just _ -> KnownWord w
      Nothing -> UnknownWord $ UnkWord w
    
instance Show Word where 
    show (KnownWord n) = n  
    show (UnknownWord (UnkWord a)) = a  
    show (EndWord) = "**END**"  
--}}}

data Production = 
    BinaryRule NT NT NT | 
    TerminalRule NT Word
    deriving (Eq, Ord)

instance Show Production where 
    show (BinaryRule x y z) = printf "%s -> %s %s" (show x) (show y) (show z) 
    show (TerminalRule x p ) = printf "%s -> %s" (show x) (show p) 

$(derive makeBinary ''NT )
$(derive makeBinary ''Word )
$(derive makeBinary ''Production )
 

-- Events 

newtype RuleEvent = RuleEvent (NT, NT)
    deriving (Eq, Ord, Binary, Show)

instance Event RuleEvent where type EventMap RuleEvent = M.Map

newtype RuleContext = RuleContext NT 
    deriving (Eq, Ord, Binary, Show)

instance Pretty RuleEvent where pPrint = text. show
instance Pretty RuleContext where pPrint = text. show

instance Context RuleContext where 
    type SubMap RuleContext = M.Map
    type Sub RuleContext = RuleContext
    decompose a = [a] 

newtype TermEvent = TermEvent Word
    deriving (Eq, Ord, Binary, Show)

instance Event TermEvent where type EventMap TermEvent = M.Map

newtype TermContext = TermContext NT 
    deriving (Eq, Ord, Binary, Show)
instance Pretty TermEvent where pPrint = text. show
instance Pretty TermContext where pPrint = text. show


instance Context TermContext where 
    type SubMap TermContext = M.Map
    type Sub TermContext = TermContext
    decompose a = [a] 

type CNFObs = (CondObserved TermEvent TermContext, 
               CondObserved RuleEvent RuleContext)

type CNFProbs = (CondDistribution TermEvent TermContext, 
                 CondDistribution RuleEvent RuleContext)

estimate :: CNFObs -> CNFProbs 
estimate (obs1, obs2) = (estimateGeneralLinear (wittenBell 5) obs1,
                         estimateGeneralLinear (wittenBell 5) obs2)

mkRuleObservation :: Production -> CNFObs
mkRuleObservation (BinaryRule a b c) = 
    (mempty, 
     condObservation (RuleEvent (b,c)) (RuleContext a))
mkRuleObservation (TerminalRule a p) = 
    (condObservation (TermEvent p) (TermContext a),
     mempty)


type RuleObs = CondObserved RuleEvent RuleContext
type RuleProbs = CondDistribution RuleEvent RuleContext

type TermObs = CondObserved TermEvent TermContext
type TermProbs = CondDistribution TermEvent TermContext


data Grammar = Grammar CNFProbs 

getProb nt (Grammar (terms, rules)) =
    case nt of 
      BinaryRule x y z -> rules (RuleContext x) (RuleEvent (y, z)) 
      TerminalRule x p  ->  terms (TermContext x) (TermEvent p) 

-- mkGrammar = Grammar . M.fromList              

-- toList (Grammar g) = M.toList g 


-- uniqueNT (Grammar g) = 
--     S.toList $ S.fromList $ map process $ M.toList g
--         where  process (BinaryRule p _ _, _) = p 
--                process (TerminalRule p _, _) = p 

-- uniqueWord (Grammar g) = 
--     S.toList $ S.fromList $ concatMap process $ M.toList g
--         where  process (BinaryRule _ _ _, _) = []
--                process (TerminalRule _ pos, _) = [pos]


