module NLP.TreeBank.Label where 

import NLP.ParseMonad
import NLP.Language.SimpleLanguage
import NLP.Model.Dependency.Format
import qualified  NLP.Model.TAG.Format as TAG
import NLP.TreeBank.TreeBank
import NLP.Grammar.Spine
import Helpers.Common

convertNTtoLabel :: ANonTerm -> ALabel
convertNTtoLabel (Atom i) = Atom i 

convertLabeltoNT :: ALabel -> ANonTerm
convertLabeltoNT (Atom i) = Atom i 

childTopLabeler :: ParseMonad Labeler
childTopLabeler = do
  pos <- toAtom $ read "POS"
  return $ labeler pos
      where labeler pos child _ = 
                convertNTtoLabel $ 
                case top $ tspine child of 
                  Just nt -> nt
                  Nothing -> pos
                
parentLabeler :: ParseMonad Labeler
parentLabeler = do
  root <- toAtom $ read "ROOT"
  return $ labeler root
      where labeler root child parent = 
                convertNTtoLabel $  
                case parent of
                  Just p  -> fromJustNote "labeler" $ lookupNonTerm (adjPos child) (tspine p)   
                  Nothing -> root

blankLabeler :: ParseMonad Labeler
blankLabeler = return (\_ _ -> Atom 1) 
                
headLabeler :: ParseMonad Labeler
headLabeler =  do
  root <- toAtom $ read "ROOT"
  pos <- toAtom $ read "POS"
  return $ labeler root pos
      where labeler root pos child parent = 
                convertNTtoLabel $  
                case parent of
                  Just p  -> fromJustDef pos $ lookupNonTerm (adjPos child - 1) (tspine p)   
                  Nothing -> root
pickSelect ::  String -> (ParseMonad Labeler, ParseMonad [ANonTerm])
pickSelect label = case label of 
               "c" -> (childTopLabeler, labels)
               "p" -> (parentLabeler, labels)
               "h" -> (headLabeler, labels)
               "n" -> (blankLabeler, return [Atom 1]) 
    where labels = (enumerate >>= (mapM toAtom))