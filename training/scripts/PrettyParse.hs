import Codec.Sexpr 
import Data.Text (replace, pack, unpack)
import System 
import qualified Data.Bimap as BM
import Helpers.Common
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
                      ( '?' , "*QUES*")
                    ]

transFor  = 
    concatMap (\c -> case BM.lookup c trans of
                                   Just n -> n
                                   Nothing -> [c])

transBack st = 
    trans' st switch
    where 
      switch = map (\(a,b) -> ([a],b) ) $ BM.toList trans
      trans' st [] = st 
      trans' st ((a,b):ls) = trans' (replace (pack b) (pack a) st) ls  


doClean (wi, l) = if l /= "" then 
    transBack $ pack $ intercalate " " $ map (dropWhile ((==) ' ')) $ lines $ advancedString $  readSexprString $ 
                transFor l  
            else pack ""

main = do
  interact (unlines .  map (unpack . doClean )  . lines)