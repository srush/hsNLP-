module PreProcess where 
import NLP.TreeBank.TreeBank
import Data.Array
import Helpers.Common
import NLP.Grammar.TAG hiding (adjPos)
import NLP.Grammar.Spine 
import Data.List

data ProcessTree = CurNode {
      nword:: WordInfo ,
      nleft::[ProcessTree],
      nright:: [ProcessTree]
} deriving (Show)

fromWIS :: WordInfoSent -> ProcessTree 
fromWIS (WordInfoSent wis) = fromWIS' baseWord 
    where sent = elems wis
          baseWord = head $ filter (((==) 0) . adjoinInd) sent
          fromWIS' wi = CurNode wi (children (< (ind wi))) (children ((ind wi) <))
              where children check = map fromWIS' $ filter (check . ind) $ filter (( == (ind wi)) . adjoinInd) sent


reInd :: ProcessTree -> ProcessTree 
reInd pt = top 
    where
    ([top], _)= reInd' ([],1) pt
    reInd'  (prev, n) (CurNode wi left right) = (prev ++ [CurNode (wi{ind = ln}) leftTree rightTree], rn)
        where 
          (leftTree, ln)  =  foldl reInd'  ([], n) left  
          (rightTree, rn) =  foldl reInd'  ([], ln+1) right  

reAdj :: ProcessTree -> ProcessTree 
reAdj pt = top 
    where
    [top]= reAdj' 0 [] pt 
    reAdj' adjInd prev (CurNode wi left right) = prev ++ [CurNode wi{adjoinInd = adjInd} leftTree rightTree]
        where 
          leftTree  =  foldl (reAdj' $ ind wi)  ([]) left  
          rightTree =  foldl (reAdj' $ ind wi)  ([]) right  

filterNode :: (WordInfo -> Bool) -> ProcessTree -> ProcessTree 
filterNode pred = filterNode'
    where 
    filterNode' (CurNode wi left right) = CurNode wi newLeft newRight 
        where 
          newLeft = map filterNode' $ concatMap clobber left
          newRight = map filterNode' $ concatMap clobber right
          clobber n@(CurNode wi l r) = 
              if pred wi then
                  (map adjust l) ++ (map adjust r)
              else [n]
                   
              where
                adjust (CurNode wi' l' r') = 
                    CurNode (wi'{sister = sister wi,
                                 adjoinInd  = adjoinInd wi}) l' r'




liftNode :: (WordInfo  -> Bool) -> ProcessTree -> ProcessTree 
liftNode pred = liftNode'
    where 
    liftNode' (CurNode wi left right) = CurNode wi newLeft newRight 
        where 
          bumpR :: [ProcessTree] -> [ProcessTree]
          bumpR [] = [] 
          bumpR [l] = [l] 
          bumpR ls = (map (\(ear, late) -> if pred $ nword ear then ear {nword = (nword ear){adjPos = adjPos $ nword $ late}}
                                         else ear) $ zip ls (tail ls)) ++ [last ls]
          bumpL [] = [] 
          bumpL [l] = [l] 
          bumpL ls = head ls : (map (\(late, ear) -> if pred $ nword ear then ear {nword = (nword ear){adjPos = adjPos $ nword $ late}}
                                         else ear) $ zip ls (tail ls)) 
          newLeft = bumpL $ map liftNode' $ concatMap lift left
          newRight = bumpR $ map liftNode' $ concatMap lift right
          lift n@(CurNode wi l r) = 
                 liftedL ++ [CurNode wi newL newR] ++ liftedR
                   
              where
                (liftedL, newL) = case l of 
                                   [] -> ([],[])
                                   (a:rest) -> if pred $ nword a then 
                                                   ([adjust a],rest)
                                               else ([],a:rest) 
                (liftedR, newR) = case r of 
                                   [] -> ([],[])
                                   rest -> if pred $ nword $ last rest then 
                                               ([adjust $ last rest], init rest)
                                           else ([], rest)

                adjust (CurNode wi' l' r') = 
                    CurNode (wi'{sister = sister wi,
                                 adjPos = adjPos wi,
                                 adjoinInd  = adjoinInd wi}) l' r'

treeUnder :: ProcessTree -> Int -> ProcessTree
treeUnder (CurNode wi l r) pos = 
    (CurNode wi (filterPos l) (filterPos r))
     where filterPos = filter (\n -> (((<) pos) $ adjPos $ nword n) || 
                                     ((((==) pos) $ adjPos $ nword n) && 
                                      (((==) Sister) $  sister $ nword n)))  

treeOver (CurNode wi l r) pos = 
    (CurNode wi (filterPos l) (filterPos r))
     where filterPos = filter (((>=) pos). adjPos . nword)  

removeEnd pred = fromJustNote "not blank" . removeEnd'
    where removeEnd' n@(CurNode wi l r) = 
              if null r then
                  if pred wi then Nothing
                  else Just n 
              else 
                  case removeEnd' $ last r of 
                    Nothing -> Just $ CurNode wi l $ init r
                    Just n -> Just $ CurNode wi l $ ((init r) ++ [n])
                      

removeFront pred = fromJustNote "not blank" . removeEnd'
    where removeEnd' n@(CurNode wi l r) = 
              if null l then
                  if pred wi then Nothing
                  else Just n 
              else 
                  case removeEnd' $ head l of 
                    Nothing -> Just $ CurNode wi (tail l) r
                    Just n -> Just $ CurNode wi (n:tail l) r
                      

                       



isNPB n = True
    --length (spineNP n) == 1
    --not ((any (hasNPorNPCC . spine . nword) $ nleft n ) || 
    --     (any (hasNPorNPCC . spine . nword) $ nright n))
  -- where 
--     spineNP node = 
--         if isPOSPossessive $ pos $ nword node then [] 
--         else 
--             (if  (hasNP . spine . nword) node then [node] else []) ++ 
--                     (concatMap (spineNP) $ nleft node) ++ 
--                     (concatMap (spineNP) $ nright node)   
    
     --havePossessive (CurNode _ l r) = any isPOSPossessive (map (pos . nword) l ++ map (pos . nword) r)  

posNP np sp = fromJustNote "posnp" $ elemIndex np $ toList sp

addNPB np npb s = mkSpine (take pos spine ++ [npb] ++ drop pos spine) 
    where pos = posNP np s
          spine = toList s

augmentNP np npb n@(CurNode wi left right) =
    if (hasNP $ spine wi) 
--       (isNPB $ treeUnder n npPos) 
    then
        CurNode (wi{spine = addNPB np npb $ spine wi} )  (aug $ reverse $ liftSide $ reverse left) (aug $ liftSide right)
    else
        CurNode wi (aug left) (aug right) 
    where 
      aug = map (augmentNP np npb) 
      liftSide ls  =  near ++ map (\c -> c {nword = (nword c){adjPos = (adjPos $ nword c) + 1} }) far 
          where (near, far) = break  (\c -> (adjPos $ nword c) > npPos || ((adjPos $ nword c) == npPos &&  (sister $ nword c) == Regular)) ls
                     
      npPos = posNP np $ spine wi
      hasNP = any ((==) np) . toList 


toWIS :: ProcessTree -> WordInfoSent 
toWIS procTree = WordInfoSent $ listArray (1, length newSent) newSent 
    where (newSent) = toWIS' (reAdj $ reInd procTree) ([])
          toWIS' (CurNode wi left right) (prev) = rightWords
              where 
                leftWords =  foldl (flip toWIS')  prev left  
                rightWords =  foldl (flip toWIS')   (leftWords ++ [wi] ) right  


