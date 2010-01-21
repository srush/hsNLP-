{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, TypeSynonymInstances #-}
module NLP.Model.Derivation where

import NLP.Model.CreateableSemi
import Helpers.Common
import NLP.Grammar.TAG 
import NLP.Model.TAGWrap
import NLP.Grammar.NonTerm
import NLP.Language
import NLP.TreeBank.TAG
import NLP.Grammar.Spine
import NLP.Grammar.Dependency

data ParseTree l = 
    Node (NonTermWrap l) [ParseTree l] | 
    Leaf (GWord l) 


niceParseTree (Leaf gword ) = 
                (text $ show $ getPOS gword) <+> (text $ show $ getLex gword) 
niceParseTree (Node nt rest) =  
    hang (text $ show nt) 3 (vcat $ map niceParseTree rest)


instance (Language l) => Show (ParseTree l) where 
    show = render.pPrint

instance (Language l) => Pretty (ParseTree l) where 
    pPrint (Leaf gword ) = 
       lparen <> (text $ show $ getPOS gword) <+> (text $ show $ getLex gword) <> rparen 
    pPrint (Node nt rest) =  
        
        (lparen <> (text $ show nt))
        <+>
        (hsep $ map pPrint rest) <> rparen


tagDerToTree (TAGDerivation tagdep ) = head c
    where
      Node _ c = convertNewTree (root rind, Sister, rind)
      rind = rootInd tagdep
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
            nt = (getNonTerm spos $ twSpine tw) 
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


                   

prior :: (TWord l ->Double)  -> Maybe (TWord l) -> Double
prior probs (Just adjstate) =  pairLikelihood
     where  
       --Prob p = (getWeight $ fromViterbi $ semi)        --(_, semi) = last $ tryEmpties findSemiProbs (adjstate, (one:: ViterbiDerivation TAGDerivation))  
       pairLikelihood = probs $ adjstate
