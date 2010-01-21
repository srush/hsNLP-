import Codec.Sexpr 
import System (getArgs) 
import System.IO
import Common
import qualified Data.Bimap as BM
import Data.Text (replace, pack, unpack)
clean tree = 
    if isAtom tree then [tree]
    else
    if nonTerm == "*QUOTE**QUOTE*" || nonTerm == "*BQUOTE**BQUOTE*" || nonTerm == "*DOT*"  then 
                 []
             else [list ((atom nonTerm):children)] 
    where nonTerm = unAtom $ head $ unList tree
          children = concatMap clean $ tail $ unList tree


trans = BM.fromList [(',' , "*COMMA*"), 
                      ( '$' , "*DOLLAR*"),
                      ( '`' , "*BQUOTE*"),
                      ( '\'' , "*QUOTE*"),
                      ( '&' , "*AMP*"),
                      ( ':' , "*COLON*"),
                      ( '\\' , "*SLASH*"),
                      ( '/' , "*FSLASH*"),
                      ( '%' , "*PERCENT*"),
                      ( ';' , "*SEMI*"),
                      ( '#' , "*POUND*"),
                     ( '.' , "*DOT*")
                    ]


transBack st = 
    trans' st switch
    where 
      switch = map (\(a,b) -> ([a],b) ) $ BM.toList trans
      trans' st [] = st 
      trans' st ((a,b):ls) = trans' (replace (pack b) (pack a) st) ls  

doClean l = if l /= "" then 
    transBack $ pack $ intercalate " " $ map (dropWhile ((==) ' ')) $ lines $ advancedString $ head $ clean  $ readSexprString $ 
                concatMap (\c -> case BM.lookup c trans of
                                   Just n -> n
                                   Nothing -> [c]) l  
            else pack ""

main = do
  interact (unlines .  map (unpack . doClean) . lines)
  