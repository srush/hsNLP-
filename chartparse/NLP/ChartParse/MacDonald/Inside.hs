{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, StandaloneDeriving #-}
module NLP.ChartParse.MacDonald.Inside where 

--{{{  Imports
import NLP.ChartParse
import Helpers.Common
import Data.List (find)
import NLP.FSM
import Data.Semiring
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Monoid.Multiplicative (times, one) 
import Control.Exception
import NLP.WordLattice
import Debug.Trace 
--}}}


type Semi fsa = FSMSemiring(State fsa)
type Sym fsa = FSMSymbol (State fsa)
type MItem fsa = Item (MSig fsa) (Semi fsa) 

data Shape = Tri | Trap 
              deriving (Eq, Ord, Show)

data Dir = R | L 
              deriving (Eq, Ord, Show)

data MSig fsa = 
    MSig {
      dir ::  Dir,
      shape :: Shape,
      headWord :: !(Sym fsa),
      sideWord :: Maybe (Sym fsa),
      sigState :: Maybe (State fsa)
    } 

--data Seal a = Sealed | Open a
--              deriving (Eq, Ord)

instance (WFSM fsa) => Show (MSig fsa) where 
    show msig = 
        intercalate " " $ [
                         show $ dir msig, 
                         show $ shape msig,
                         show $ headWord msig,
                         show $ sideWord msig
                        ] 
deriving instance (WFSM fsa) => Eq (MSig fsa) 
deriving instance (WFSM fsa) => Ord (MSig fsa) 


type Seal a = Maybe a

isSealed (Just a) = False
isSealed _ = True

unSeal (Just a) = a 

unsealed = Just 
sealed = Nothing

-- Just Two shapes
-- Trap R = Tri R <- Tri L    
-- Trap L = Tri R -> Tri L    
-- Tri R =  Trap R + Tri R
-- Tri L =  Trap L + Tri L
 


--{{{  MSig Classes
-- expandMSig sp =  (dir sp, shape sp, headWord sp, sigState sp) --   TODO- fix back, 

-- instance (WFSM fsa) => Eq (MSig fsa) where 
--     (==)  = (==) `on` expandMSig

-- instance (WFSM fsa) => Ord (MSig fsa) where 
--     compare = compare `on` expandMSig
--}}}


-- Advances an internal WFSM (equivalent in this model to "adjoining" a new
-- dependency. 
-- advance :: (WFSM fsa) => Maybe (State fsa) -> Int -> Sym fsa -> 
--            [(State fsa, Semi fsa)] 
-- advance st split nextWord  = 
--     assert (not $ isSealed st) $ do 
--     (newState, p) <- next (unSeal st) nextWord split 
--     return (newState, p) 
 


-- The OptLink Rules take spans with dual head (0,0) and adjoin the head on 
-- one side to the head on the other. 
--optLinkL'
optL :: (WFSM fsa) => Int -> (MSig fsa) -> (MSig fsa) -> [MItem fsa]
optL split rtri ltri = do 
    --trace (show rtri ++ show ltri ++ show split) $ do
      guard $ not $ isSealed $ sigState ltri 
      guard $ isSealed $ sigState rtri
      (newState, p) <- next (unSeal $ sigState ltri) (headWord rtri) split
      return $ (ltri { 
                  shape = Trap,
                  sideWord = Just $ headWord rtri,
                  sigState = unsealed newState
                },
                p)


optR :: (WFSM fsa) => Int -> (MSig fsa) -> (MSig fsa) -> [MItem fsa]
optR split rtri ltri = do
  --trace (show "opt r" ++ show rtri) $ do  
      guard $ (not $ isSealed $ sigState rtri)
      guard $ isSealed $ sigState ltri
      (newState, p) <- next (unSeal $ sigState rtri) (headWord ltri) (split)
      return $ (rtri { 
                  shape = Trap,
                  sideWord = Just $ headWord ltri,
                  sigState = unsealed newState
                },
               p)


combineL :: (WFSM fsa) => (MSig fsa) -> (MSig fsa) -> [MItem fsa]
combineL tri trap = 
  --trace (show "combine l" ++ show trap) $ 
  assert ((shape tri) == Tri && (dir tri == L)) $
  assert ((shape trap) == Trap && (dir trap == L)) $
  do 
    guard $ trapOuterWord == triWord
    guard $ isSealed $ sigState tri
    return $ (MSig {
                 dir = L,
                 shape = Tri,
                 headWord = headWord trap,
                 sideWord = Nothing,
                 sigState = sigState trap
               }, one)
  where 
     triWord = headWord tri
     trapOuterWord = fromJustNote "trap must have outer" $ sideWord trap

combineR :: (WFSM fsa) => (MSig fsa) -> (MSig fsa) -> [MItem fsa]
combineR trap tri = 
  --trace (show "combine r" ++ show trap) $ 
  assert ((shape trap) == Trap && (dir trap == R)) $
  assert ((shape tri) == Tri && (dir tri == R)) $
          do 
            guard $ triWord == trapOuterWord
            guard $ isSealed $ sigState tri
            return $ (MSig {
                 dir = R,
                 shape = Tri,
                 headWord = headWord trap,
                 sideWord = Nothing,
                 sigState = sigState trap
               }, one)
  where 
     triWord = headWord tri
     trapOuterWord = fromJustNote "trap must have outer" $ sideWord trap


seal :: (WFSM fsa) => Span ->  (MItem fsa) -> [MItem fsa]
seal (i, k) item@(sig,semi) = 
    --trace (show "seal" ++ show (i,k)) $ 
    item : do
      guard $ Tri == shape sig
      guard $ not $ isSealed $ sigState sig
      let split = if dir sig == L then i else k 
      Just semi' <- [finish (fromJustNote "unseal" $ sigState sig) split]  
      
      return $ (sig { 
                  sigState = Nothing
                 }, 
                semi `times` semi')

     
    
genCombine :: (WFSM fsa) => ((MSig fsa) -> (MSig fsa) -> [MItem fsa]) -> MItem fsa -> MItem fsa -> [MItem fsa]
genCombine comb (sig1, semi1) (sig2, semi2) = do 
  (sig', semi') <- comb sig1 sig2
  return (sig', semi' `times` semi1 `times` semi2) 

tryOpt :: (WFSM fsa) => Int -> (MItem fsa) -> (MItem fsa) -> [MItem fsa]
tryOpt split item1@(sig1, semi1) item2@(sig2, semi2) = 
    case ((shape sig1, dir sig1), (shape sig2, dir sig2)) of 
         ((Tri, R), (Tri, L)) ->  (genCombine (optL split) item1 item2) ++ 
                                  (genCombine (optR split) item1 item2)
         _ -> []

tryCombine :: (WFSM fsa) => (MItem fsa) -> (MItem fsa) -> [MItem fsa]
tryCombine item1@(sig1, semi1) item2@(sig2, semi2) = 
    case ((shape sig1, dir sig1), (shape sig2, dir sig2)) of 
      ((Trap, R), (Tri, R)) -> genCombine combineR item1 item2
      ((Tri, L), (Trap, L)) -> genCombine combineL item1 item2
      _ -> []

-- Seed 
seed :: (WFSM fsa) => 
        GetFSM fsa -> 
        Int ->
       [Sym fsa] -> 
       InitialDerivationRule (MItem fsa)
seed getFSA i sym1s = do --trace (show "seed" ++ show i) $ do  
      sym1 <- sym1s 
      let (leftFSA, rightFSA) = getFSA i sym1
      dir <- [R,L]
      (state, semi ) <- initialState $ if dir ==R then rightFSA else leftFSA
      return (MSig {
                dir = dir,
                sigState = Just state,
                shape = Tri,
                sideWord = Nothing,
                headWord = sym1 
              }, 
              semi) 

type GetFSM fsa = Int -> Sym fsa -> (fsa, fsa) --todo: fix this 

processCell :: ( Show (FSMSemiring (State fsa)), WFSM fsa, WordLattice sent) => 
               GetFSM fsa -> 
               sent ->  
               (Symbol sent -> Sym fsa) ->     
               ([MItem fsa] -> [MItem fsa]) ->            
               Span -> -- Size of the cell 
               (Span -> [MItem fsa]) -> -- function from cell to contenst 
               ([MItem fsa] -> [MItem fsa]) -> 
               [MItem fsa] -- contents of the new cell 
processCell getFSA sentence wordConv beamFirst (i, k) chart beam = -- trace (show (i,k)) $ 
   if k > latticeLength sentence then []
   else
    concat $ map (seal (i,k)) $ 
    if k == i then        
        beam $ beamFirst $ seed getFSA i $
                 map wordConv $ getWords sentence i
    else
        let newtraps = finish
                 [tryOpt j s1 s2
                      | j  <- [i..k-1],
                        s2 <- chart (j+1,k),
                        s1 <- chart (i,j)]
            newtris = finish
                 [tryCombine s1 s2
                      | j  <- [i..k],
                        s2 <- if (j,k) == (i,k) then newtraps else chart (j,k),
                        s1 <- if (i,j) == (i,k) then newtraps else chart (i,j)]
        in newtraps ++ newtris
    where finish = beam . M.toList . M.fromListWith mappend . concat

macdonaldParse :: ( Show (FSMSemiring (State fsa)), WFSM fsa, WordLattice sent) => 
               GetFSM  fsa -> 
               (Symbol sent -> Sym fsa) -> 
               sent  ->                 
              (Span -> M.Map (MSig fsa) (Semi fsa) -> M.Map (MSig fsa) (Semi fsa)) ->
              ([MItem fsa] -> [MItem fsa]) -> 
              ([MItem fsa] -> [MItem fsa]) -> 
               (Maybe (Semi fsa), Chart (MSig fsa) (Semi fsa))
macdonaldParse getFSM wordConv sent prune beam beamFirst = (semi, chart)  
    where chart = chartParse sent (processCell getFSM sent wordConv beamFirst) prune beam
          semi = do
            last <- chartLookup (1, latticeLength sent) chart
            let semiresults = map snd $ filter accept last
            
            guard (length semiresults > 0)  
            return $ mconcat semiresults 

accept :: (WFSM fsa) => MItem fsa -> Bool
accept (sig, _) = 
    (shape sig == Tri) && (dir sig == L) && (isSealed $ sigState sig) 
