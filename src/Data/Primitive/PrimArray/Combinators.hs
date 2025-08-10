{-|
Module: Data.Primitive.PrimArray.Combinators
Description: Exports combinators used in mutable builders.
Copyright: (c) Andrey Prokopenko, 2025
License: BSD-3-Clause
Stability: experimental
-}
module Data.Primitive.PrimArray.Combinators where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Vector.Generic.Mutable  (MVector)
import GHC.Stack (HasCallStack)

import Data.DAWG.Internal.BaseType

import qualified Data.Primitive.PrimArray as A
import qualified Data.Vector.Generic.Mutable  as V

-- | Alias for 'MutablePrimArray' @s@ 'Int'.
type IntArray s = A.MutablePrimArray s Int

-- | Infix version of @unsafeRead@.
(!~) :: (MVector v a, PrimMonad m, HasCallStack) => v (PrimState m) a -> BaseType -> m a
(!~) xs !i = V.read xs (fromIntegral i)

-- | Infix version of @unsafeWrite@.
(<~~) :: (MVector v a, PrimMonad m, HasCallStack) => v (PrimState m) a -> BaseType -> a -> m ()
(<~~) xs !i x = V.write xs (fromIntegral i) x

-- | Infix version of @modify@. Modifies element at the given position.
(!<~~) :: (MVector v a, PrimMonad m, HasCallStack) => v (PrimState m) a -> BaseType -> (a -> a) -> m ()
(!<~~) xs !i f = do
  !x <- xs !~ i
  let !nx = f x
  xs <~~ i $! nx

-- | Infix version of @writePrimArray@.
(<~) :: (PrimMonad m) => A.MutablePrimArray (PrimState m) Int -> Int -> Int -> m ()
(<~) = A.writePrimArray

-- | Infix version of @readPrimArray@.
(!) :: (PrimMonad m) => A.MutablePrimArray (PrimState m) Int -> Int -> m Int
(!) = A.readPrimArray 
