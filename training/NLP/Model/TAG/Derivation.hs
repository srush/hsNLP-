{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module NLP.Model.TAG.Derivation where

import NLP.Model.CreateableSemi
import Helpers.Common
import NLP.Grammar.TAG 
import NLP.Model.TAG.Wrap
import NLP.Model.TAG.Format
import NLP.Grammar.Spine
import NLP.Grammar.Dependency
import NLP.Model.TAG.Semi
import NLP.Model.ParseTree
import Data.Tree
import NLP.ParseMonad

tagDerToTree :: TAGDerivation -> ParseMonad PTree
tagDerToTree (TAGDerivation tagdep ) = do
  let rind = rootInd tagdep
  start <- root rind
  let Node _ c = convertNewTree (start, Sister, rind)
  return $ ParseTree $ head c
    where
      flat = flattenDep $ tagdep
      convertNewTree (tw, typ, ind)=
          convertToTree' tw spos ind 
          where
            spos = ((lastOfSpine $ twSpine tw) -1)
      convertToTree' tw (-1) _ = Node (Right (twWord tw)) [] 
      convertToTree' tw spos ind = 
          buildLeft rightSide leftSide
          where
            (_, (left, right)) = flat !! (ind -1)
            atSpos = map (\adj -> (dcWord $ adjInfo adj, adjType adj, adjPos adj)). catMaybes.  
                     fromMaybe [] . lookup spos . alignWithSpine (twSpine tw)
            nt = (getNonTerm spos $ twSpine tw) 
            leftSide = adjlevels (reverse $ atSpos left) []
            rightSide =  adjlevels (reverse $ atSpos right) []
            buildLeft right [last] = buildRight right last 
            buildLeft right (cur:ls) = Node (Left nt) $ (map convertNewTree cur ++ [buildLeft right ls]) 
            buildRight [last] left = finalConvert (left, last)
            buildRight (cur:ls) left = Node (Left nt) $ (buildRight ls left: (reverse $ map convertNewTree cur))
            finalConvert (left, right)= Node (Left nt) $ 
                  (map convertNewTree $ left) ++ 
                  ([convertToTree' tw (spos-1) ind]) ++ 
                  (map convertNewTree $ reverse $ right)
 
      
      adjlevels side start = if null rest then [rights] else (rights++[head rest]):(adjlevels (tail rest) [] ) 
          where 
            rights = takeWhile (\(_,typ,_) -> typ /= Regular) side
            rest = dropWhile (\(_,typ,_) -> typ /= Regular) side

prior :: (TWord -> Double)  -> Maybe TWord -> Double
prior probs (Just adjstate) =  pairLikelihood
     where  
       --Prob p = (getWeight $ fromViterbi $ semi)        --(_, semi) = last $ tryEmpties findSemiProbs (adjstate, (one:: ViterbiDerivation TAGDerivation))  
       pairLikelihood = probs $ adjstate
