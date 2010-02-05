module LP.DualDecomp where 

import Helpers.Common hiding (Result)
import qualified Data.Map as M 
import qualified Data.Set as S
import Debug.Trace

alpha = 0.2

type Lambda a = M.Map a Double 
type Result a = S.Set a 

update :: (Ord a, Show a) => Int -> Result a -> Result a -> Lambda a -> Lambda a 
update i y1 y2 lambda = trace ("Adds: " ++ show adds) $ trace ("Subs:" ++ show subs) $ 
    insertManyWith (+) ( alpha/(1 + (fromIntegral $ round ((fromIntegral i)/10)))) (S.toList adds) $   
    insertManyWith (+) (-alpha/(1 + (fromIntegral $ round ((fromIntegral i)/10)))) (S.toList subs) lambda  
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
      getMAP :: Lambda node -> Int -> IO (Double, S.Set node, Maybe a)
    }
               
data DecompState = Done | InProcess

data Master node a = 
    Master {
      slaves :: (Slave node a,
                 Slave node a),
      --theta :: Theta node,
      lambda :: Lambda node,
      step :: Int,
      obj :: [(Double,Double)],
      state :: DecompState,
      lastResults :: (Maybe a, Maybe a) 
    } 

instance (Show node) => Show (Master  node a) where  
    show master = 
        show (step master) ++ " " ++  show (lambda master) 

initialize slaves = 
     Master {slaves = slaves, 
--             theta  = theta ,
             lambda = M.empty,
             step   = 0, 
             obj    = [],
             state  = InProcess,
             lastResults = (Nothing, Nothing)
             } 

takeStep :: (Show a, Ord a) => Master a b -> IO (Master a b)
takeStep master = do 
  ((obj1, obj2) ,(map1, map2), (best1, best2)) <- maps
  putStrLn $ "round" ++ show i
  return $ master {
    lambda = update i map1 map2 (lambda master),
    step = i + 1,
    obj = (obj1, obj2):(obj master),
    state = if map1 == map2 then Done else InProcess,
    lastResults = (best1, best2) 
  } 
    where
      (slave1, slave2) = slaves master
      maps = do 
        (obj1, map1, best1) <- (getMAP slave1)  (lambda master) i 
        (obj2, map2, best2) <- (getMAP slave2)  (lambda master) i
        return $ trace (show map1 ++ show map2) $ ((obj1,obj2), (map1, map2), (best1, best2)) 
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