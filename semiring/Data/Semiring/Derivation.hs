{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, UndecidableInstances #-}
module Data.Semiring.Derivation (Derivation(..), MultiDerivation(..), DualDerivation(..), mkDerivation, fromDerivation, mkDualDerivation, fromDualDerivation) where
import Data.Semiring
import Data.Semiring.Helpers
import qualified Data.Set as S 
import Data.Monoid
import Data.Maybe (isNothing)
import Control.Exception

-- | The 'Derivation' semiring keeps track of a single path or derivation 
--   that led to the known output. If there are more than one path it discards 
--   in favor the lesser path (based on ord). The main purpose of this semiring 
--   is to track derivations for ViterbiNBestDerivation. If you want to keep all paths, 
--   use 'MultiDerivation'.
--
--   Derivation takes a Monoid as an argument that describes how to build up paths or 
--   more complicated structures.  
newtype Derivation m = Derivation (Maybe m)
    deriving (Eq, Ord) 

instance (Monoid m) => Multiplicative (Derivation m) where
    one = Derivation $ Just mempty
    times (Derivation d1) (Derivation d2) = Derivation $ do 
        d1' <- d1
        d2' <- d2
        return $ mappend d1' d2'

instance Monoid (Derivation m) where 
    mempty = Derivation Nothing
    mappend (Derivation s1) (Derivation s2) = 
        Derivation $ case (s1,s2) of 
                       (Nothing, s2) -> s2
                       (s1, Nothing) -> s1
                       (s1, s2) -> s1

instance (Monoid m) => Semiring (Derivation m)

instance (Show m) => Show (Derivation m) where 
    show (Derivation (Just m)) = show m 
    show (Derivation Nothing) = "[]" 

mkDerivation :: (Monoid m ) => m -> Derivation m 
mkDerivation = Derivation . Just  

fromDerivation :: (Monoid m ) => Derivation m -> m 
fromDerivation (Derivation (Just m)) = m  
fromDerivation (Derivation Nothing) = throw $ AssertionFailed "no derivation"  


-- | The 'MultiDerivation' semiring keeps track of a all paths or derivations 
--   that led to the known output. This can be useful for debugging output.
-- 
--  Keeping all these paths around can be expensive. 'MultiDerivation' leaves open 
--  the implementation of the internal path monoid for more compact representations. 
newtype MultiDerivation m = MultiDerivation (S.Set m)
    deriving (Eq, Show, Ord) 

instance (Monoid m, Ord m) => Multiplicative (MultiDerivation m) where
    one = MultiDerivation $ S.fromList [mempty]
    times (MultiDerivation d1) (MultiDerivation d2) = MultiDerivation $ 
        S.fromList $ 
        map (uncurry mappend) $ 
        cartesian (S.toList d1) (S.toList d2) 

instance (Ord m) => Monoid (MultiDerivation m) where 
    mempty = MultiDerivation S.empty
    mappend (MultiDerivation s1) (MultiDerivation s2) = MultiDerivation $ S.union s1 s2

instance (Ord m, Monoid m, Eq m) => Semiring (MultiDerivation m)



newtype DualDerivation m1 m2 = DualDerivation (m1 m2)
    deriving (Eq, Show, Ord) 

instance (Monad m1, Monoid (m1 m2), Monoid m2) => Multiplicative (DualDerivation m1 m2) where
    one = DualDerivation $ return mempty
    times (DualDerivation d1) (DualDerivation d2) = 
        DualDerivation $ do
          d1' <- d1
          d2' <- d2
          return $ mappend d1' d2'

instance (Monoid (m1 m2), Monoid m2) => Monoid (DualDerivation m1 m2) where 
    mempty = DualDerivation mempty
    mappend (DualDerivation s1) (DualDerivation s2) = DualDerivation $ s1 `mappend` s2

instance (Monad m1, Monoid (m1 m2), Monoid m2) => Semiring (DualDerivation m1 m2)



mkDualDerivation = DualDerivation   

fromDualDerivation (DualDerivation m) = m  

