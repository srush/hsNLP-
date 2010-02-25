module Memoize (memoize, memofix) where

import qualified Data.Map as Data.Map
import System.IO.Unsafe
import Control.Concurrent.MVar

memoize :: Ord a => (a -> b) -> (a -> b) 
memoize f =
    unsafePerformIO $
    do cacheRef <- newMVar Data.Map.empty
       return (\x -> unsafePerformIO (g cacheRef x))
    where
      g cacheRef x = 
          do cache <- readMVar cacheRef
             case Data.Map.lookup x cache of
               Just y  -> return y
               Nothing -> 
                   do let y     = f x 
                      let cache = Data.Map.insert x y cache
                      swapMVar cacheRef cache
                      return y

memofix f = let g = memoize (f g) in g
