import Codec.Sexpr 
import System (getArgs) 
import System.IO
import Helpers.Common
import qualified Data.Bimap as BM
import Data.Text (replace, pack, unpack)
import NLP.TreeBank.TreeBank
import Data.Array
import NLP.Language.SimpleLanguage
import Debug.Trace
import NLP.ParseMonad hiding (unAtom)


isWord tree = 
    if isList tree then
        (isAtom $ head $ unList tree) &&  (isAtom $ head $ tail $ unList tree)
    else
        False

addBackWords (WordInfoSent wis) tree = 
    list ([atom "TOP"] ++  (tr ++ map getAtomN [k..m])) 
            
    where 
      (tr, k) = count ([], 1) tree
      (1,m) = bounds wis
      getWord n =  --trace (show n) $  
                   wis ! n  
      count :: ([Sexpr String], Int) -> Sexpr String -> ([Sexpr String], Int) 
      count (ls, n) tree  = 
          if isAtom tree then
              (ls ++ [tree], n)
          else if isWord tree && isList tree then
              let [pos, word] =  map unAtom $ unList tree
                  newn = getNewN pos n
              in 
              (ls ++ getAtomsBefore pos n ++ [getAtom pos n], newn+1)
          else
              let (child,n') = foldl count ([], n) $ unList tree in
              (ls ++ [list child], n')
      getAtomsBefore pos' n =
          --trace (show "before" ++show n++ show pos') $  
          if not $ pos' `elem` (map (transFor. render .pPrint) $ posStr $ getWord n) then
              (getAtomN n) : getAtomsBefore pos' (n+1)
          else
              []
      getAtomN n =           
          list [atom $ transFor $ render $ pPrint $ head $ posStr $ getWord n, 
                atom $ transFor $ unWord $ wordStr $ getWord n ]

      getAtom pos' n = 
          --trace (show "norm" ++show n++ show pos') $  
          if not $ pos' `elem` (map (transFor. render .pPrint) $ posStr $ getWord n) then
              getAtom pos' (n+1)
          else 
              getAtomN n
      getNewN pos' n =          
          --trace (show "new" ++show n++ show pos' ++ show (posStr $ getWord n)) $  
          if not $ (pos' `elem` (map (transFor. render .pPrint) $ posStr $ getWord n)) then
              --trace "moving on" $ 
              getNewN pos' (n+1)
          else
              n

      unWord (Word word) = word
clean tree = 
    if isAtom tree then [tree]
    else
    if nonTerm == "NPB" || dropWhile ((/=) '_') nonTerm == "_CC" then 
                 children 
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
    transBack $ pack $ intercalate " " $ map (dropWhile ((==) ' ')) $ lines $ advancedString $ addBackWords wi $ head $ clean $ readSexprString $ 
                transFor l  
            else pack ""

main = do
  [testFile] <- getArgs
  contents <- readFile testFile
  let sents = (parseSentences  testFile contents) :: ParseMonad [WordInfoSent] 
  dm <- loadMappers $ defaultLoadMap{shouldCollapseWords = False} 
  let newsents = runParseMonad sents dm  
  interact (unlines .  map (unpack . doClean ) . zip newsents . lines)
  