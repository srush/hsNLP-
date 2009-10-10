{-# LANGUAGE TypeSynonymInstances, TypeFamilies, FlexibleInstances, DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell, FlexibleContexts #-}  
module TAGparse where 

import TAG 
import Adjunction
import Data.Monoid
import Data.Maybe (fromJust, isJust)
import Control.Monad (guard)
import NLP.Semiring
import NLP.Semiring.Derivation
import NLP.Semiring.Prob
import NLP.Semiring.Viterbi
import NLP.Semiring.ViterbiNBestDerivation
import NLP.FSM.Simple
import NLP.FSM
import NLP.ChartParse
import NLP.ChartParse.Eisner
import DependencyStructure
import Data.Maybe (catMaybes, fromMaybe)
import Control.Exception
import Data.DeriveTH hiding (Derivation)
import Data.Binary hiding (Word)
import Control.Monad (liftM, ap)
import Safe (fromJustNote, fromJustDef) 
import Data.Function (on)
import Sentence
import Text.PrettyPrint.HughesPJClass
import NonTerm
import qualified Data.Map as M
import qualified Data.Set as S
import POS
import Debug
import Distance
import Debug.Trace
type SpineExist = M.Map POS (S.Set Spine)

instance (Pretty d, Monoid d ) => Pretty (Derivation d) where
    pPrint = pPrint . fromDerivation 
    

type TAGCountSemi = Derivation TAGCounts 

data DerivationCell = DerivationCell {
      dcWord :: TAGWord
} deriving (Eq, Show)

mkDerivationCell word = 
    DerivationCell word 



newtype TAGDerivation = TAGDerivation (Dependency (AdjunctionInfo (DerivationCell)), [(Adjunction, ProbDebug)]) 
    deriving (Eq, Monoid)

tagDerToTree (TAGDerivation (tagdep,_)) = head c
    where
      Node _ c = convertNewTree (root rpos, Sister, rpos)
      rpos = rootPos tagdep
      flat = flattenDep $ tagdep
      convertNewTree (tw, typ, ind)=
          convertToTree' tw spos ind 
          where
            spos = ((lastOfSpine $ twSpine tw) -1)
      convertToTree' tw (-1) _ = Leaf (twWord tw) 
      convertToTree' tw spos ind = 
          buildLeft rightSide leftSide
          where
            (_, (left, right)) = flat !! (ind -1)
            atSpos = map (\adj -> (dcWord $ adjInfo adj, adjType adj, adjPos adj)). catMaybes.  
                     fromMaybe [] . lookup spos . alignWithSpine (twSpine tw)
            nt = getNonTerm spos $ twSpine tw
            leftSide = adjlevels (reverse $ atSpos left) []
            rightSide =  adjlevels (reverse $ atSpos right) []
            buildLeft right [last] = buildRight right last 
            buildLeft right (cur:ls) = Node nt $ (map convertNewTree cur ++ [buildLeft right ls]) 
            buildRight [last] left = finalConvert (left, last)
            buildRight (cur:ls) left = Node nt $ (buildRight ls left: (reverse $ map convertNewTree cur))
            finalConvert (left, right)= Node nt $ 
                  (map convertNewTree $ left) ++ 
                  ([convertToTree' tw (spos-1) ind]) ++ 
                  (map convertNewTree $ reverse $ right)
 
      
      adjlevels side start = if null rest then [rights] else (rights++[head rest]):(adjlevels (tail rest) [] ) 
          where 
            rights = takeWhile (\(_,typ,_) -> typ /= Regular) side
            rest = dropWhile (\(_,typ,_) -> typ /= Regular) side
                    


instance Show TAGDerivation where 
    show = render . pPrint

instance Pretty TAGDerivation where 
    pPrint (TAGDerivation der) = 
        (text $ show der)
             

mkTestTAGWord :: SpineExist -> (Int, GWord, (Bool, Bool)) -> [TAGWord]
mkTestTAGWord counts (ind, (word,pos), comma) = 
     map (\sp -> mkTAGWord (word,pos) sp comma ind) $ S.toList $ 
     fromJustDef mempty $ M.lookup pos counts

viterbiHelp prob td = 
    mkViterbi $ Weighted (Prob prob, mkDerivation $ TAGDerivation td)

type TAGFSM semi = GraphWFSM Int (Maybe TAGWord) semi
type GetSemi word edge semi =  (Int,word) -> Maybe (Int,word) -> edge -> semi

-- tagSentenceFSMs (TAGSentence sent dep)  = 
--     map (\(i, e) -> makeTagFSM (getWord sent) dep i e) $
--     flattenDep dep

-- -- | Assert- left is reverse order, right is in order
-- makeTagFSM getWordInfo dep headi (left, right)  = 
--     (makeDirFSM ALeft left, makeDirFSM ARight right)
--     where  makeDirFSM side expObs= 
--                       expectFSMTag $
--                                     [((getWordInfo.adjPos) `liftM` childInd,
--                                        mkDerivation $ countAdjunction $ 
--                                        mkAdjunction getWordInfo tword
--                                              (fmap (getWordInfo.adjPos) childInd) 
--                                              pos (maybe Sister adjType childInd) side
--                                              isAdjacent 
--                                      ) 
--                                      | (pos, adjs)  <- aligned,
--                                        childInd <- adjs
--                                     ]
--                       where aligned = alignWithSpine (twSpine tword) expObs 
--            tword = getWordInfo headi

-- expectFSMTag transitions = 
--     makeWFSM 0 n (Just Nothing) ((n, []):
--            [ (i-1, [(trans, (i, semi))])
--             | ((trans,semi), i) <- zip transitions [1..]])  
--         where n = length transitions



data AdjState model = 
    AdjState { 
      stateModel :: model,
      stateHead :: !TAGWord,
      statePos :: !Int, 
      stateSide :: !AdjunctionSide,
      stateDisCache :: DisCache,
      -- stateDistance :: Distance,
      --stateContext :: AdjunctionParent

      stateHasAdjunction :: Bool,
      stateAfterComma :: Bool
} 

initAdj :: model ->
           DisCache ->
           AdjunctionSide ->
           TAGWord ->
           AdjState model 
initAdj model discache side tagword = 
    fromJustNote "nonblank" $ mkEmpty 0 $ lastOfSpine $ twSpine tagword
    where 
      mkEmpty i last = 
          if i > last then Nothing  
          else Just $ AdjState {
                    stateModel = model,
                    stateHead = tagword,
                    stateSide = side, 

                    statePos = i,
                    stateDisCache = discache,
                    
                    stateHasAdjunction = False,
                    stateAfterComma = False

                    --stateDistance = mempty{ numComma = case side of 
                    --                                    ALeft  -> if fst $ twNearComma $ tagword then OneComma else NoComma
                    --                                    ARight -> if snd $ twNearComma $ tagword then OneComma else NoComma},
                    -- stateContext = undefined
                   }

stateNT adjstate = getNonTerm (statePos adjstate) $ twSpine (stateHead adjstate)

-- cacheContext adjstate = adjstate {stateContext = 
--                                       mkParent (stateHead adjstate) 
--                                                (statePos adjstate) 
--                                                (stateSide adjstate)
--                                                (stateDistance adjstate)
--                                  }

stateSpine adjstate = twSpine $ stateHead adjstate

prior :: (TAGWord ->Double)  -> Maybe TAGWord -> Double
prior probs (Just adjstate) =  pairLikelihood
     where  
       --Prob p = (getWeight $ fromViterbi $ semi) 
       --(_, semi) = last $ tryEmpties findSemiProbs (adjstate, (one:: ViterbiDerivation TAGDerivation))  
       pairLikelihood = probs $ adjstate

{-# INLINE expandAdjState #-}
expandAdjState as =  (statePos as, stateHasAdjunction as, stateAfterComma as)


instance Eq (AdjState a) where 
    (==) = (==) `on` expandAdjState

instance Ord (AdjState a) where
    {-# INLINE compare #-}
    compare = compare `on` expandAdjState

instance Show (AdjState a) where 
    show = show . statePos


tryEmpties findSemi split (adjstate, semi) = (adjstate, semi):
    if isFinal adjstate then [] 
    else
        case semi' of 
          Nothing -> []
          Just semi' -> 
              tryEmpties findSemi split (adjstate {statePos = statePos adjstate + 1, 
                                                   stateHasAdjunction = False,
                                                   stateAfterComma = False}, 
                                         semi `times` semi')
        where
          semi' = findSemi adjstate split End Sister (mkDistance adjstate split)
          
generalNext :: (Semiring semi, FSMState (AdjState a) ) => 
               (AdjState a -> Int -> AdjunctionAction (TAGWord) -> AdjunctionType -> Distance -> Maybe semi) -> 
               (AdjState a) ->  
               (Maybe (TAGWord)) ->
               Int -> --huge hack!
               [(AdjState a, semi)] 
generalNext findSemi adjstate child split  = 
        if isFinal adjstate then [] 
        else if stateNT adjstate == rootNT then
                 case findSemi adjstate split (DoAdj $ fromJustNote "its good" child) Sister (mkDistance adjstate split) of 
                   Nothing -> []
                   Just s -> 
                       [(adjstate {statePos = statePos adjstate + 1}, 
                         s)]
        else
            concatMap 
            (tryEmpties findSemi (split+ (if stateSide adjstate == ALeft then (-1) else 1) )) $ 
            catMaybes $
            [ doOneAdj Sister one (mkDistance adjstate split),
              doOneAdj Regular one ((mkDistance adjstate split))-- {firstAdjunction = True,
                                                                    --afterComma = False})
            ]
              
        
            where doOneAdj atype baseSemi dis = 
                      case findSemi adjstate split (DoAdj $ fromJust child) atype dis of
                        Nothing -> Nothing 
                        Just s -> Just (adjstate{stateAfterComma =  
                                                twIsComma $ fromJustNote "real" child, 
                                            stateHasAdjunction =  True},
                                  baseSemi `times` s)

findSemiProbs adjstate split child atype dis = 
    if not $ valid head (maybeAdj child) pos atype then 
        Nothing
    else 
        Just $ case child of 
             End -> 
                 viterbiHelp p
                      (Dependency mempty, if debug then 
                                              [(adj, probAdjunctionDebug adj probs)] else [])
             Reg -> 
                 viterbiHelp p
                      (Dependency mempty, if debug then 
                                              [(adj, probAdjunctionDebug adj probs)] else [])
             DoAdj child' ->
                 viterbiHelp p
                      (singletonDep (twInd head) (twInd child') $ 
                       (AdjunctionInfo pos atype (mkDerivationCell child')), 
                       if debug then [(adj, probAdjunctionDebug adj probs)] else [])
        where
          parent =  mkParent head pos side atype dis
          adj = mkAdjunction parent child atype 
          p = probAdjunction adj probs
          AdjState {stateSide  = side, 
                    statePos   = pos,
                    stateHead  = head,
                    stateModel = (probs,valid),
                    stateDisCache = discache,
                    stateHasAdjunction = hasAdj,
                    stateAfterComma = afterComma
                    } = adjstate


mkDistance adjstate split = Distance 
                            (not $ stateHasAdjunction adjstate)
                            verb
                            (stateAfterComma adjstate)
    where (verb, comma) = ((stateDisCache adjstate) (twInd $ stateHead adjstate, split))
                            

data  TAGTree a =  TAGTree {leftForest :: AdjState a, 
                            rootWord :: TAGWord, 
                            rightForest :: AdjState a}
                deriving (Show, Eq, Ord)
adjStateFinal adjstate = statePos adjstate == (lastOfSpine $ twSpine $ stateHead adjstate) 

type Validity = TAGWord -> (Maybe TAGWord) -> Int -> AdjunctionType -> Bool

instance WFSM (AdjState (TAGProbs, Validity)) where 
    type State (AdjState (TAGProbs, Validity)) = (AdjState (TAGProbs, Validity)) 
    initialState init = tryEmpties findSemiProbs (twInd $ stateHead init)  (init, one)

instance FSMState (AdjState (TAGProbs,Validity)) where
    type FSMSymbol (AdjState (TAGProbs, Validity)) = Maybe (TAGWord)
    type FSMSemiring (AdjState (TAGProbs, Validity)) = ViterbiDerivation TAGDerivation
    next = generalNext findSemiProbs
    isFinal = adjStateFinal  

findSemiCounts adjstate split child atype dis = 
    if not $ valid model head (maybeAdj child) pos atype then 
        Nothing
    else
        Just $ mkDerivation $ countAdjunction $ mkAdjunction parent child atype
        where
          parent = mkParent head pos side atype dis 
          AdjState {stateSide  = side, 
                    statePos   = pos,
                    stateHead  = head,
                    stateModel = model,
                    stateDisCache = discache,
                    stateHasAdjunction = hasAdj,
                    stateAfterComma = afterComma
                    } = adjstate

instance WFSM (AdjState TAGSentence) where 
    type State (AdjState TAGSentence) = (AdjState TAGSentence) 
    initialState init = tryEmpties findSemiCounts (twInd $ stateHead init) (init, one)

instance FSMState (AdjState TAGSentence)  where
    type FSMSymbol (AdjState TAGSentence) = Maybe (TAGWord)
    type FSMSemiring (AdjState TAGSentence) = Derivation TAGCounts 
    next = generalNext findSemiCounts 
    isFinal = adjStateFinal 
