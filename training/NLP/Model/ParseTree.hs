{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module NLP.Model.ParseTree where 
import Data.Tree
import Data.Traversable
import Data.Foldable
import NLP.Language.SimpleLanguage
import NLP.ParseMonad
import Helpers.Common

newtype ParseTree a = ParseTree (Tree a)
    deriving (Functor, Foldable, Traversable)

type PTree = ParseTree (Either ANonTerm GWord)

niceParseTree (ParseTree t) = npt t
    where npt  (Node (Right (word,pos)) [] ) = 
              (text $ show $ pos) <+> (text $ show $ word) 
          npt (Node (Left nt) rest) =  
              hang (text $ show nt) 3 (vcat $ map npt rest)


instance Show (ParseTree (Either NonTerm (Word,POS))) where 
    show = render.pPrint

instance Pretty (ParseTree (Either NonTerm (Word,POS))) where 
    pPrint (ParseTree t) = phelp t 
        where 
          phelp (Node (Right (word,pos)) []) = 
              lparen <> (pPrint pos) <+> (pPrint $ word) <> rparen 
          phelp (Node (Left nt) rest) =  
              (lparen <> (pPrint  nt))
              <+>
              (hsep $ map phelp rest) <> rparen
