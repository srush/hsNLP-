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
      Node _ c = convertNewTree (root rpos, rpos)
      rpos = rootPos tagdep
      flat = flattenDep $ tagdep
      convertNewTree (tw, ind)= 
              convertToTree' tw ((lastOfSpine $ twSpine tw) -1) ind
      convertToTree' tw (-1) _ = Leaf (twWord tw) 
      convertToTree' tw spos ind = 
              Node (getNonTerm spos $ twSpine tw) $ 
                   (reverse $ map convertNewTree $ atSpos left) ++ 
                   ([convertToTree' tw (spos-1) ind]) ++ 
                   (map convertNewTree $ atSpos right)
                  where  (_, (left, right)) = flat !! (ind -1)
                         atSpos = map (\adj -> (dcWord $ adjInfo adj, adjPos adj) ). catMaybes.  
                                  fromMaybe [] . lookup spos . alignWithSpine (twSpine tw)


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
      stateDisCache :: DisCache
      -- stateDistance :: Distance,
      --stateContext :: AdjunctionParent
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
                    stateDisCache = discache
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
expandAdjState as =  (statePos as)


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
              tryEmpties findSemi split (adjstate {statePos = statePos adjstate + 1}, semi `times` semi')
        where
          semi' = findSemi adjstate split Nothing Sister 
          
generalNext :: (Semiring semi, FSMState (AdjState a) ) => 
               (AdjState a -> Int -> Maybe (TAGWord) -> AdjunctionType ->  Maybe semi) -> 
               (AdjState a) ->  
               (Maybe (TAGWord)) ->
               Int -> --huge hack!
               [(AdjState a, semi)] 
generalNext findSemi adjstate child split  = 
        if isFinal adjstate then [] 
        else if stateNT adjstate == rootNT then 
           [(adjstate {statePos = statePos adjstate + 1}, 
             fromJust $ findSemi adjstate split child Sister)]
        else
            concatMap (tryEmpties findSemi (split+ (if stateSide adjstate == ALeft then (-1) else 1) )) $ do
              atype <- [Sister, Regular]
              let semi = findSemi adjstate split child atype
              guard $ isJust semi 
              return $ (adjstate,
                        fromJust semi)

findSemiProbs adjstate split child atype = 
    Just $ case child of 
             Nothing -> 
                 viterbiHelp p
                                 (Dependency mempty, if debug then [(adj, probAdjunctionDebug adj probs)] else [])
             Just child' ->
                 viterbiHelp p
                      (singletonDep (twInd head) (twInd child') $ 
                       (AdjunctionInfo pos atype (mkDerivationCell child')), 
                       if debug then [(adj, probAdjunctionDebug adj probs)] else [])
        where
          
          parent =  mkParent head pos side (discache (twInd head, split)  ) 
          adj = mkAdjunction parent child atype
          p = probAdjunction adj probs
          AdjState {stateSide  = side, 
                    statePos   = pos,
                    stateHead  = head,
                    stateModel = probs,
                    stateDisCache = discache
                                 
                    } = adjstate

data  TAGTree a =  TAGTree {leftForest :: AdjState a, 
                            rootWord :: TAGWord, 
                            rightForest :: AdjState a}
                deriving (Show, Eq, Ord)
adjStateFinal adjstate = statePos adjstate == (lastOfSpine $ twSpine $ stateHead adjstate) 

instance WFSM (AdjState TAGProbs) where 
    type State (AdjState TAGProbs) = (AdjState TAGProbs) 
    initialState init = tryEmpties findSemiProbs (twInd $ stateHead init)  (init, one)

instance FSMState (AdjState TAGProbs) where
    type FSMSymbol (AdjState TAGProbs) = Maybe (TAGWord)
    type FSMSemiring (AdjState TAGProbs) = ViterbiDerivation TAGDerivation
    next = generalNext findSemiProbs
    isFinal = adjStateFinal  

findSemiCounts adjstate split child atype = 
    if not $ valid model head child pos atype then 
        Nothing
    else
        Just $ mkDerivation $ countAdjunction $ mkAdjunction parent child atype
        where
          parent = mkParent head pos side (discache (twInd head, split)) 
          AdjState {stateSide  = side, 
                    statePos   = pos,
                    stateHead  = head,
                    stateModel = model,
                    stateDisCache = discache
                    } = adjstate

instance WFSM (AdjState TAGSentence) where 
    type State (AdjState TAGSentence) = (AdjState TAGSentence) 
    initialState init = tryEmpties findSemiCounts (twInd $ stateHead init) (init, one)

instance FSMState (AdjState TAGSentence)  where
    type FSMSymbol (AdjState TAGSentence) = Maybe (TAGWord)
    type FSMSemiring (AdjState TAGSentence) = Derivation TAGCounts 
    next = generalNext findSemiCounts 
    isFinal = adjStateFinal 
