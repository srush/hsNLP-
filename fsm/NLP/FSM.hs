{-# LANGUAGE TypeFamilies, FlexibleContexts #-} 
module NLP.FSM where 
import NLP.Semiring


class (FSMState (State a)) => WFSM a where 
    type State a 
    initialState :: a -> [(State a, FSMSemiring (State a))]

--stateid symbol semiring = WFSA {
--      initialState :: FSMState stateid symbol semiring
--    }

class (Ord a, Semiring (FSMSemiring a), 
       Show a, Show (FSMSymbol a), 
       Ord (FSMSymbol a)) => 
    FSMState a where
    type FSMSymbol a 
    type FSMSemiring a  
    next :: a -> FSMSymbol a -> [(a, FSMSemiring a)]
    isFinal :: a -> Bool
    



