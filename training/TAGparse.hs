{-# LANGUAGE TypeSynonymInstances, TypeFamilies, FlexibleInstances, DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell, FlexibleContexts, UndecidableInstances  #-}  
module TAGparse where 

import Common hiding (Derivation)
import TAG 
import Adjunction
import Data.Monoid
import Data.Maybe (fromJust)
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
import Control.Exception
import Sentence
import NonTerm
import qualified Data.Map as M
import qualified Data.Set as S
import POS
import Debug
import Distance
import ExtraParams

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
             

mkTestTAGWord :: SpineExist -> (Int, GWord) -> [TAGWord]
mkTestTAGWord counts (ind, (word,pos)) = 
     map (\sp -> mkTAGWord (word,pos) sp ind) $ S.toList $ 
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



data AdjState model semi = 
    AdjState { 
      stateModel :: model,
      stateHead :: !TAGWord,
      statePos :: !Int, 
      stateSide :: !AdjunctionSide,
      stateDisCache :: DisCache,
      -- stateDistance :: Distance,
      --stateContext :: AdjunctionParent

      stateCurDelta :: Delta,
      stateIsAfterComma :: Bool,

      stateNPBLast :: Maybe GWord, 

      stateCollinsRule :: Bool,
      stateParents :: M.Map (Int, VerbDistance, Delta) AdjunctionParent 
} 

initAdj :: model ->
           DisCache ->
           AdjunctionSide ->
           TAGWord ->
           Bool -> 
           AdjState model semi 
initAdj model discache side tagword collins = 
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
                    
                    stateCurDelta = startDelta,
                    stateIsAfterComma = False,

                    stateNPBLast = Nothing, 

                    stateCollinsRule = collins,
                    stateParents = M.fromList [((pos,verb, delta), 
                                                mkParent tagword Nothing pos side verb delta) | 
                                    pos <- [0..last-1],
                                    verb <- [minBound..maxBound ],
                                    delta <- [minBound.. maxBound]
                                   ]
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
expandAdjState as =  (statePos as, stateCurDelta as, stateIsAfterComma as, stateNPBLast as, stateSide as)


instance Eq (AdjState a b) where 
    (==) = (==) `on` expandAdjState

instance Ord (AdjState a b) where
    {-# INLINE compare #-}
    compare = compare `on` expandAdjState



tryEmpties findSemi split fin (adjstate, semi) = (adjstate, semi):
    if (statePos adjstate + (if fin then 0 else 1)) >= (lastOfSpine $ twSpine $ stateHead adjstate) then [] 
    else
        case semi' of 
          Nothing -> []
          Just semi' -> 
              if (prevComma $ stateCurDelta adjstate) || 
                 ((stateCollinsRule adjstate) &&
                  (stateSide adjstate == ARight) &&
                  ((stateIsAfterComma adjstate) && 
                   (not $ snd $ (stateDisCache adjstate) (twInd $ stateHead adjstate, split))))
              then 
                  []
              else

                  tryEmpties findSemi split fin (adjstate {statePos = statePos adjstate + 1, 
                                                   stateCurDelta = startDelta,
                                                   stateIsAfterComma = False,
                                                   stateNPBLast = Nothing
                                                  }, 
                                         semi `times` semi')
        where
          semi' = findSemi adjstate End Sister (mkDistance adjstate split)
          

generalFinish findSemi adjstate split  = --trace (show adjstate) $
    if curPos == lastPos then semi else mempty
    where  (endstate, semi) = last $ tryEmpties findSemi split True (adjstate, one)
           lastPos =  lastOfSpine $ twSpine $ stateHead endstate
           curPos = statePos endstate
-- generalNext :: (Semiring semi, FSMState (AdjState a) ) => 
--                (AdjState a -> Int -> AdjunctionAction (TAGWord) -> AdjunctionType -> Distance -> Maybe semi) -> 
--                (AdjState a) ->  
--                (Maybe (TAGWord)) ->
--                Int -> --huge hack!
--                [(AdjState a, semi)] 
generalNext findSemi adjstate child split  = 
    if (statePos adjstate) >= (lastOfSpine $ twSpine $ stateHead adjstate)  then [] 
       
    else if stateNT adjstate == rootNT then
             if stateSide adjstate == ARight then
                 []
             else
                 case findSemi adjstate (DoAdj $ fromJustNote "its good" child) Sister (mkDistance adjstate split) of 
                   Nothing -> []
                   Just s -> 
                       [(adjstate {statePos = (statePos adjstate) + 1} , s)]
                 
   else
      if stateCollinsRule adjstate &&
         stateIsAfterComma adjstate &&
         (stateSide adjstate == ARight) &&
         (not $ snd $ (stateDisCache adjstate) (twInd $ stateHead adjstate, split))  then
       --collins comma trick
       --trace ((show split)++ (show $ stateHead adjstate) ++ (show $ child) ) $ []
        []
      else
           catMaybes $ do 
             (adjstate',semi) <- tryEmpties findSemi split False (adjstate, one)
             atype <- [Sister, Regular]
             return $  doOneAdj findSemi semi  adjstate' child atype $ mkDistance adjstate' split
            

doOneAdj findSemi baseSemi adjstate child atype dis =                           
    case findSemi adjstate (DoAdj $ fromJust child) atype dis of
      Nothing -> Nothing 
      Just s -> Just (adjstate{stateCurDelta = newDelta,
                               stateIsAfterComma = prevComma oldDelta,
                               stateNPBLast = if npbMode then 
                                                  if twIsComma child' then
                                                      stateNPBLast adjstate
                                                  else Just $ twWord $ fromJustNote "read" child 
                                              else Nothing
                              }, baseSemi `times` s)
          where   
            npbMode = isNPB (stateNT adjstate)
            child' = fromJustNote "real" child
            oldDelta = stateCurDelta adjstate
            newDelta = if twIsComma child' then
                           oldDelta {prevComma = True} 
                       -- else if twIsConj child' then 
                       --     oldDelta {prevConj = True}
                       else Delta {prevComma = False,
                                   prevConj  = False,
                                   adjacent  = False}

validProbsComma adjstate child atype =
    (snd $ stateModel adjstate) (stateHead adjstate) child (statePos adjstate) atype  


findSemiProbs adjstate child atype vdis = 
    if not $ valid head (maybeAdj child) pos atype then 
        Nothing
    else 
        Just $ mkSemi p head child pos atype
        where
          parent = case npblast of
                     Nothing -> fromJust $ M.lookup (pos, vdis, curDelta) parents
                     Just _ ->
                         mkParent head npblast pos side vdis curDelta
                   
          adj = mkAdjunction parent child atype 
          p = probAdjunction adj probs
          AdjState {stateSide  = side, 
                    statePos   = pos,
                    stateHead  = head,
                    stateModel = ProbModel probs _ valid,
                    stateDisCache = discache,
                    stateCurDelta = curDelta,
                    stateNPBLast = npblast,
                    stateParents = parents 
                    } = adjstate


mkDistance adjstate split = VerbDistance 
                            --(not $ stateHasAdjunction adjstate)
                            verb
                            
    where (verb, _) = ((stateDisCache adjstate) (twInd $ stateHead adjstate, split))
                            

adjStateFinal adjstate = statePos adjstate >= ((lastOfSpine $ twSpine $ stateHead adjstate) - 1) 

type Validity = TAGWord -> (Maybe TAGWord) -> Int -> AdjunctionType -> Bool

data ProbModel = ProbModel {
      probs :: TAGProbs,
      extra :: FlipProbs,
      validity :: Validity 
    }

class CreateableSemi a where 
    mkSemi :: Double -> TAGWord -> (AdjunctionAction TAGWord) -> Int -> AdjunctionType -> a
    mkSemiSmall :: Double -> a
instance CreateableSemi Prob where 
    mkSemi p head child pos atype  = 
        Prob p
    mkSemiSmall p = Prob p 

instance CreateableSemi (ViterbiDerivation TAGDerivation) where 
    mkSemiSmall p = viterbiHelp p (Dependency mempty, [])
    mkSemi p head child pos atype  = 
        case child of 
             End -> 
                 viterbiHelp p
                      (Dependency mempty, [])
--                       if debug then [(adj, probAdjunctionDebug adj probs)] else [])
             DoAdj child' ->
                 
                 viterbiHelp p
                      (singletonDep (twInd head) (twInd child') $ 
                          (AdjunctionInfo pos atype (mkDerivationCell child')), [])   
  --                     if debug then [(adj, probAdjunctionDebug adj probs)] else []) 



instance (CreateableSemi semi, Semiring semi) => WFSM (AdjState ProbModel semi) where 
    type State (AdjState ProbModel semi) = (AdjState ProbModel semi) 
    initialState init = if (lastOfSpine $ twSpine $ stateHead init) == 0 then [(init, one)]
                        else
                            [(init{stateSide = ALeft }, 
                          mkSemiSmall (probFlip flipprobs ALeft $ flip)),
                         (init{stateSide = ARight},
                          mkSemiSmall (probFlip flipprobs ARight $ flip)
                          )]
        where flipprobs = extra $ stateModel init
              flip = mkFlip (stateHead init) (stateSide init)

    
instance (CreateableSemi semi, Semiring semi) => FSMState (AdjState ProbModel semi) where
    type FSMSymbol (AdjState ProbModel semi) = Maybe (TAGWord)
    type FSMSemiring (AdjState ProbModel semi) = semi -- ViterbiDerivation TAGDerivation
    next = generalNext findSemiProbs 
    finish = generalFinish findSemiProbs       
    isFinal = const True  


validCountComma adjstate child atype =
    valid (stateModel adjstate) (stateHead adjstate) child (statePos adjstate) atype  

findSemiCounts adjstate child atype vdis = 
    if not $ valid model head (maybeAdj child) pos atype then 
        Nothing
    else
        Just $ mkDerivation $ countAdjunction $ mkAdjunction parent child atype 
        where
          parent = mkParent head npblast pos side vdis curDelta
          AdjState {stateSide  = side, 
                    statePos   = pos,
                    stateHead  = head,
                    stateModel = model,
                    stateDisCache = discache,
                    stateCurDelta = curDelta,
                    stateNPBLast = npblast
                    } = adjstate

instance WFSM (AdjState TAGSentence a) where 
    type State (AdjState TAGSentence a) = (AdjState TAGSentence a) 
    initialState init = [(init, one)]

instance FSMState (AdjState TAGSentence a)  where
    type FSMSymbol (AdjState TAGSentence a) = Maybe (TAGWord)
    type FSMSemiring (AdjState TAGSentence a) = Derivation TAGCounts 
    next = generalNext findSemiCounts 
    isFinal = const True
    finish = generalFinish findSemiCounts 
instance Show (AdjState a b) where 
    show s = (show $ statePos s) ++ (show $ ((lastOfSpine $ twSpine $ stateHead s) - 1) ) ++ (show $ stateSide s)


