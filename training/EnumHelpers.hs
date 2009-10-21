{-# LANGUAGE TemplateHaskell, BangPatterns #-}
module EnumHelpers where 
import Data.List
import Test.QuickCheck
import Data.DeriveTH

computeMaxBound :: [Int] -> Int
computeMaxBound ls = (product (map (+ 2) ls)) - 1  

{-# INLINE combineEnum #-}
combineEnum :: [(Int, Int)] -> Int
combineEnum enumInt = loop enumInt 1 0
    where 
      loop [] _ result = result
      loop ((!enum, !factor): rest) !prod !result = loop rest (prod * (factor +2)) (result + (enum +1) * prod)
      
uncombineEnum :: Int -> [Int] -> [Int] 
uncombineEnum combInt bounds = map (\a -> a - 1) $  uncomb combInt bounds 
        where
          uncomb 0 _ = [] 
          uncomb _ [] = [] 
          uncomb cur (b:bounds) = 
              ((cur) `mod` (b+2)) : uncomb (cur `div` (b+2)) bounds  
      
newtype Bound = Bound (Int, Int)
    deriving Show

instance Arbitrary Bound where 
    arbitrary = do 
      b <- choose (1,500)
      a <- choose (0, b)
      return $ Bound (a,b)

prop_combineEnum a = ((product $ map fromIntegral $ take 8 bounds) < fromIntegral ((maxBound::Int))) ==> 
                     enum == uncombineEnum comb bounds  
    where types = (a :: [Bound])
          b = map (\ (Bound (a', b')) -> (a', b')) $ take 8 a 
          (enum, bounds) = unzip b
          comb  = combineEnum $ b
       
{-# INLINE mkFromEnum2 #-}
mkFromEnum2 :: (Enum a, Enum b, Bounded a, Bounded b) => (a, a) -> (b,b) -> Int   
mkFromEnum2 (a, maxA) (b, maxB) = combineEnum [(fromEnum a, fromEnum $ maxA),  
                                               (fromEnum b, fromEnum $ maxB)]

mkToEnum2 :: (Enum a, Enum b, Bounded a, Bounded b) =>  (a,b) -> Int -> (a,b)   
mkToEnum2 (maxA, maxB) n = (toEnum a, toEnum b) 
    where
      [a, b] = uncombineEnum n [fromEnum maxA, 
                                fromEnum maxB]

{-# INLINE mkFromEnum3 #-}
mkFromEnum3 :: (Enum a, Enum b, Enum c, Bounded a, Bounded b, Bounded c) => 
               (a, a) -> (b,b) -> (c,c) -> Int
mkFromEnum3 (a, maxA) (b, maxB) (c,maxC) = combineEnum [(fromEnum a, fromEnum $ maxA),  
                                                        (fromEnum b, fromEnum $ maxB), 
                                                        (fromEnum c, fromEnum $ maxC)
                                                       ]

   
mkToEnum3 :: (Enum a, Enum b, Enum c, Bounded a, Bounded b, Bounded c) =>  
             (a,b,c) ->Int -> (a,b,c)   
mkToEnum3 (maxA, maxB, maxC) n = (toEnum a, toEnum b, toEnum c) 
    where [a,b,c] = uncombineEnum n [fromEnum maxA, fromEnum maxB, fromEnum maxC]



mkFromEnum4 :: (Enum a, Enum b, Enum c, Enum d,
                Bounded a, Bounded b, Bounded c, Bounded d) => 
               (a, a) -> (b,b) -> (c,c) -> (d,d) -> Int
mkFromEnum4 (a, maxA) (b, maxB) (c,maxC) (d,maxD) = 
    combineEnum [(fromEnum a, fromEnum $ maxA),  
                 (fromEnum b, fromEnum $ maxB), 
                 (fromEnum c, fromEnum $ maxC),
                 (fromEnum d, fromEnum $ maxD)
                ]

mkToEnum4 :: (Enum a, Enum b, Enum c, Enum d, Bounded a, Bounded b, Bounded c, Bounded d) =>  
             (a,b,c,d) ->Int -> (a,b,c,d)   
mkToEnum4 (maxA, maxB, maxC, maxD) n = (toEnum a, toEnum b, toEnum c, toEnum d) 
    where [a,b,c,d] = uncombineEnum n [fromEnum maxA, fromEnum maxB, fromEnum maxC, fromEnum maxD]


{-# INLINE mkFromEnum5 #-}
mkFromEnum5 :: (Enum a, Enum b, Enum c, Enum d, Enum e)  => 
               (a, a) -> (b,b) -> (c,c) -> (d,d) -> (e,e) -> Int
mkFromEnum5 (a, maxA) (b, maxB) (c,maxC) (d,maxD) (e,maxE) = 
    combineEnum [(fromEnum a, fromEnum $ maxA),  
                 (fromEnum b, fromEnum $ maxB), 
                 (fromEnum c, fromEnum $ maxC),
                 (fromEnum d, fromEnum $ maxD),
                 (fromEnum e, fromEnum $ maxE)]

mkToEnum5 :: (Enum a, Enum b, Enum c, Enum d, Enum e)  =>
             (a,b,c,d,e) ->Int -> (a,b,c,d,e)    
mkToEnum5 (maxA, maxB, maxC, maxD, maxE) n = (toEnum a, toEnum b, toEnum c, toEnum d, toEnum e) 
    where [a,b,c,d,e] = uncombineEnum n [fromEnum maxA, fromEnum maxB, fromEnum maxC, fromEnum maxD, fromEnum maxE]

mkToEnum6 :: (Enum a, Enum b, Enum c, Enum d, Enum e, Enum f)  =>
             (a,b,c,d,e,f) ->Int -> (a,b,c,d,e,f)    
mkToEnum6 (maxA, maxB, maxC, maxD, maxE, maxF) n = (toEnum a, toEnum b, toEnum c, toEnum d, toEnum e, toEnum f) 
    where [a,b,c,d,e,f] = uncombineEnum n [fromEnum maxA, fromEnum maxB, fromEnum maxC, fromEnum maxD, fromEnum maxE, fromEnum maxF]

{-# INLINE mkFromEnum6 #-}
mkFromEnum6 :: (Enum a, Enum b, Enum c, Enum d, Enum e, Enum f)  => 
               (a, a) -> (b,b) -> (c,c) -> (d,d) -> (e,e) -> (f,f)-> Int
mkFromEnum6 (a, maxA) (b, maxB) (c,maxC) (d,maxD) (e,maxE) (f, maxF) = 
    combineEnum [(fromEnum a, fromEnum $ maxA),  
                 (fromEnum b, fromEnum $ maxB), 
                 (fromEnum c, fromEnum $ maxC),
                 (fromEnum d, fromEnum $ maxD),
                 (fromEnum e, fromEnum $ maxE),
                 (fromEnum f, fromEnum $ maxF)]

mkToEnum7 :: (Enum a, Enum b, Enum c, Enum d, Enum e, Enum f, Enum g)  =>
             (a,b,c,d,e,f,g) ->Int -> (a,b,c,d,e,f,g)    
mkToEnum7 (maxA, maxB, maxC, maxD, maxE, maxF, maxG) n = (toEnum a, toEnum b, toEnum c, toEnum d, toEnum e, toEnum f, toEnum g) 
    where [a,b,c,d,e,f,g] = uncombineEnum n [fromEnum maxA, fromEnum maxB, fromEnum maxC, fromEnum maxD, fromEnum maxE, fromEnum maxF, fromEnum maxG]

{-# INLINE mkFromEnum7 #-}
mkFromEnum7 :: (Enum a, Enum b, Enum c, Enum d, Enum e, Enum f, Enum g)  => 
               (a, a) -> (b,b) -> (c,c) -> (d,d) -> (e,e) -> (f,f)-> (g,g)-> Int
mkFromEnum7 (a, maxA) (b, maxB) (c,maxC) (d,maxD) (e,maxE) (f, maxF) (g, maxG) = 
    combineEnum [(fromEnum a, fromEnum $ maxA),  
                 (fromEnum b, fromEnum $ maxB), 
                 (fromEnum c, fromEnum $ maxC),
                 (fromEnum d, fromEnum $ maxD),
                 (fromEnum e, fromEnum $ maxE),
                 (fromEnum f, fromEnum $ maxF),
                 (fromEnum g, fromEnum $ maxG)]


mkFromEnumList :: (Enum a, Bounded a) => [a] -> a -> Int
mkFromEnumList  ls  max = combineEnum $ zip (map fromEnum ls) (repeat $ fromEnum max)

mkToEnumList :: (Enum a, Bounded a) => a -> Int -> [a]
mkToEnumList max n = map toEnum $ uncombineEnum n $ repeat $ fromEnum max


checkEnum :: (Eq a, Enum a) => a -> Bool
checkEnum a =  a == (toEnum $ fromEnum a)


data Simple = A | B | C 
              deriving (Enum, Bounded, Eq, Show)
data SimplePlus = SimplePlus Simple Char
                deriving (Show)

$( derive makeArbitrary ''SimplePlus ) 
$( derive makeArbitrary ''Simple ) 

instance Enum SimplePlus where 
    fromEnum (SimplePlus s c) = mkFromEnum2 (s, maxBound) (c, maxBound) 
    toEnum n = SimplePlus s c
        where (s,c) = mkToEnum2 (maxBound, maxBound) n

prop_simplePlusEnum a = checkEnum
    where types = (a::SimplePlus) 

