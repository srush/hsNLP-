module LP.DualDecomp where 

import Helpers.Common hiding (Result)
import qualified Data.Map as M 
import qualified Data.Set as S
import Debug.Trace

alpha = 0.5

type Lambda a = M.Map a Double 
type Result a = S.Set a 

update :: (Ord a, Show a) => Int -> Result a -> Result a -> Lambda a -> Lambda a 
update i y1 y2 lambda = trace ("Adds: " ++ show adds) $ trace ("Subs:" ++ show subs) $ 
    insertManyWith (+) (alpha) (S.toList adds) $   
    insertManyWith (+) (-alpha) (S.toList subs) lambda  
    where 
      ratio= (fromIntegral $ S.size y1) / (fromIntegral $ S.size y2)
      adds = S.difference y2 y1
      subs = S.difference y1 y2
      insertManyWith fn v ls m = foldl (\m k -> M.insertWith fn k v m) m ls 

type Theta node = node -> Double  

combine theta lambda dif scale node =
    theta node `dif` lambdaDif  
    where lambdaDif  = scale * (fromJustDef 0.0 $ M.lookup node lambda)
          
data Slave node =
    Slave {
      getMAP :: Lambda node -> Int -> IO (S.Set node)
    }
               
data Master theta node = 
    Master {
      slaves :: (Slave node,
                 Slave node),
      theta :: Theta node,
      lambda :: Lambda node,
      step :: Int 
    } 

instance (Show node) => Show (Master theta node) where  
    show master = 
        show (step master) ++ " " ++  show (lambda master) 

initialize theta slaves = 
     Master {slaves = slaves, 
             theta  = theta,
             lambda = M.empty,
             step   = 0 
            } 

takeStep master = do 
  (map1, map2) <- maps
  return $ master {
    lambda = update i map1 map2 (lambda master),
    step = i + 1
  } 
    where
      (slave1, slave2) = slaves master
      maps = do 
        map1 <- (getMAP slave1)  (lambda master) i 
        map2 <- (getMAP slave2)  (lambda master) i
        return $ (map1, map2) 
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