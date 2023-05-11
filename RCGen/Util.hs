{-# LANGUAGE CPP #-}

module Util where

#if !MIN_VERSION_base(4,8,0)
-- Prelude in base < 4.8 does not include the Foldable class
import Data.Foldable (Foldable)
#endif
#if !MIN_VERSION_base(4,6,0)
-- instance Functor ((->) a) has been moved from Control.Monad.Instances to Base in base-4.6.0
import Control.Monad.Instances()
#endif

import qualified Data.Foldable as F
import qualified Data.List as List

import Debug.Trace

-- Print message s and show value x, returning r
dbg s x r = Debug.Trace.trace (s++(show x)) r

-- Print message s, show and return value x
dbgs s x = dbg s x x

-- printing strings in ShowS
ps :: String -> ShowS
ps = (++)

-- fold items into a ShowS, using some function returning a ShowS
folds :: Foldable t => (a -> ShowS) -> t a -> ShowS
folds f = F.foldr (\a -> (.) $ f a) id

-- fold items into a ShowS, and intersperse the items with an additional element
-- First convert it to a [ShowS] (because t may not be a list but, e.g., a Map k, for which List.intersperse is not defined)
foldsIntersperse :: Foldable t => ShowS -> (a -> ShowS) -> t a -> ShowS
foldsIntersperse el f = List.foldr (.) id . List.intersperse el . F.foldr (\x -> (f x :)) []

-- apply a function compisition to two or more argument arguments
(.:) :: (y -> z) -> (a -> b -> y) -> a -> b -> z
(.:) = fmap . fmap

(.:.) :: (y -> z) -> (a -> b -> c -> y) -> a -> b -> c -> z
(.:.) = fmap . fmap . fmap

(.::) :: (y -> z) -> (a -> b -> c -> d -> y) -> a -> b -> c -> d -> z
(.::) = fmap . fmap . fmap . fmap

