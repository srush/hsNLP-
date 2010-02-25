module NLP.CNF.Coordinate where 

--{{{  Imports
import Helpers.Common hiding (Result)
import qualified Data.Map as M 
import qualified Data.Set as S
import LP.DualDecomp
import Debug.Trace
--}}}


-- | Here's the idea, we want to do a viterbi parse, 
-- we want to then take the results, and feed them to a graph optimization 
-- and then we want to cycle. We're going to call this parseCutting for now 

data Updater node = Updater {
      getLambda :: Penalty node -> Int -> IO (Penalty node)}

type Penalty node = node -> Double

penalize :: Theta node -> Penalty node -> Theta node
penalize theta penalty node = theta node + penalty node 
            

data Coordinate node = Coordinate {
      bbox :: (Updater node, Updater node),
      cLambda :: Penalty node,
      cRound :: Int
} 

initCoordinate bboxes = 
    Coordinate {
      bbox = bboxes,
      cLambda = const 0,
      cRound = 0 
   }

runCoordOne (Coordinate (b1,b2) lambda i) = do 
    lambda' <- getLambda b1 lambda i
    lambda'' <- getLambda b2 lambda' i
    return $ Coordinate {
                 bbox = (b1,b2),
                 cLambda = lambda'',
                 cRound = i + 1
               }

