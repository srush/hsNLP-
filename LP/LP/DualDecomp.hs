module LP.DualDecomp where 

import Helpers.Common hiding (Result)
import qualified Data.Map as M 
import qualified Data.Set as S

type Lambda a = M.Map a Double 
type Result a = S.Set a 

update :: (Ord n, Show n) => Master n a b -> Int -> Result n -> Result n -> Lambda n -> Lambda n 
update master i y1 y2 lambda = 
    insertManyWith (+) (decayRate master (alpha master) i) (S.toList adds) $   
    insertManyWith (+) (- (decayRate master (alpha master) i)) (S.toList subs) lambda  
    where 
      --ratio= (fromIntegral $ S.size y1) / (fromIntegral $ S.size y2)
      adds = S.difference y2 y1
      subs = S.difference y1 y2
      insertManyWith fn v ls m = foldl (\m k -> M.insertWith fn k v m) m ls 

type Theta node = node -> Double  

combine theta lambda dif scale node =
    theta node `dif` lambdaDif  
    where lambdaDif  = scale * (fromJustDef 0.0 $ M.lookup node lambda)
          
data Slave node a=
    Slave {
      getMAP :: Lambda node -> Int -> IO (Double, S.Set node, a)
    }
               
data DecompState = Done Int | InProcess

data Master node a b = 
    Master {
      slaves :: (Slave node a ,
                 Slave node b),
      ratio  :: Double,
      --theta :: Theta node,
      lambda :: Lambda node,
      step :: Int,
      obj :: [(Double, Double)],
      state :: DecompState,
      lastMAP :: S.Set node,
      lastMAPs :: (S.Set node,S.Set node),

      resultHistory :: [(a, b)], 
      alpha :: Double,
      decayRate :: Double -> Int -> Double
    } 

instance (Show node) => Show (Master node a b) where  
    show master = 
        show (step master) ++ " " ++  show (lambda master) 

initialize slaves ratio alpha decay = 
     Master {slaves = slaves, 
--             theta  = theta ,
             lambda = M.empty,
             step   = 0, 
             obj    = [],
             state  = InProcess,
             resultHistory = [],
             lastMAP= mempty,
             lastMAPs = (mempty, mempty),
             ratio  = ratio,
             alpha = alpha, 
             decayRate = decay
             } 

getRoundConverged master = 
    case state master of 
      Done i -> Just i
      InProcess -> Nothing

takeStep :: (Show n, Ord n) => Master n a b -> IO (Master n a b)
takeStep master = do 
  ((obj1, obj2) ,(map1, map2), (best1, best2)) <- maps
  --putStrLn $ "round" ++ show i
  return $ master {
    lambda = update master i map1 map2 (lambda master),
    step = i + 1,
    obj = (obj1, obj2):(obj master),
    state = if map1 == map2 then Done i else InProcess,
    resultHistory = (best1, best2):resultHistory master, 
    lastMAP = S.difference map1 map2,
    lastMAPs = (S.difference map1 map2,
                S.difference map2 map1)
  } 
    where
      (slave1, slave2) = slaves master
      maps = do 
        (obj1, map1, best1) <- (getMAP slave1)  (lambda master) i 
        (obj2r, map2, best2) <- (getMAP slave2)  (M.map (* ratio master) (lambda master)) i
        let obj2 = obj2r / ratio master
        return $ ((obj1,obj2), (map1, map2), (best1, best2)) 
      i = step master
      

finalMAP master =
    (getMAP slave1) (lambda master) (step master)
    where 
      (slave1,_ ) = slaves master

bothMAP master = do
        map1 <- (getMAP slave1) (lambda master) (step master)
        map2 <- (getMAP slave2) (lambda master) (step master)
        return $ (map1, map2)
    where 
      (slave1,slave2) = slaves master