module LP.DualDecomp where 

import Helpers.Common hiding (Result)
import qualified Data.Map as M 
import qualified Data.Set as S


alpha = 0.5



type Lambda a = M.Map a Double 
type Result a = S.Set a 

update :: (Ord a) => Result a -> Result a -> Lambda a -> Lambda a 
update y1 y2 lambda =
    insertManyWith (+) alpha (S.toList adds) $   
    insertManyWith (+) (-alpha) (S.toList subs) lambda  
    where 
      adds = S.difference y1 y2
      subs = S.difference y2 y1
      insertManyWith fn v ls m = foldl (\m k -> M.insertWith fn k v m) m ls 

type Theta node = node -> Double  

combine theta lambda dif node =
    theta node `dif` lambdaDif  
    where lambdaDif  = fromJustDef 0.0 $ M.lookup node lambda
          
data Slave node =
    BlackBox {
      getMAP :: Theta node -> S.Set node
    }

data Master theta node = 
    Master {
      slaves :: (Slave node,
                 Slave node),
      theta :: Theta node,
      lambda :: Lambda node,
      step :: Int 
    } 

initialize theta slaves = 
     Master {slaves = slaves, 
             theta  = theta,
             lambda = M.empty,
             step   = 0 
            } 

takeStep master =
    master {
      lambda = update map1 map2 (lambda master),
      step = step master + 1
    } 
    where
      (slave1, slave2) = slaves master
      (map1, map2) = ((getMAP slave1) (combine (theta master) (lambda master) (+)),
                      (getMAP slave2) (combine (theta master) (lambda master) (-)))
      