{-|
Module: Data.DAWG.Internal.Stack
Description: Exports stack data as well as its internal API.
Copyright: (c) Andrey Prokopenko, 2025
License: BSD-3-Clause
Stability: experimental
-}
module Data.DAWG.Internal.Stack where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Foldable
import Data.Primitive.MutVar

-- ** Stack

-- | Represents mutable stack. Used to build DAWG in 'Data.DAWG.Internal.DAWGBuilder.DAWGBuilder'.
-- Implements subset of @std::stack@ API.
newtype Stack m a = StackRef { getStackRef :: MutVar m (Stack_ a) }

-- | Represents immutable stack with explicit data constructors.
-- | For mutable version see 'Stack'.
data Stack_ a
  = Elem !a !(Stack_ a)
  | EndOfStack
  deriving Foldable

instance Show a => Show (Stack_ a) where
  show = show . reverse . toList

-- | Pushes given element to the stack.
push :: forall a m. PrimMonad m => a -> Stack (PrimState m) a -> m ()
push v ref =
  let go = \case
        EndOfStack -> Elem v EndOfStack
        Elem !prev !st -> Elem v (Elem prev st)
  in modifyMutVar' (getStackRef ref) go
{-# INLINE push #-}

-- | Gets a single element from the given immutable stack if it is present.
-- Returns 'Nothing' otherwise. For mutable version see 'top'.
top_ :: Stack_ a -> Maybe a
top_ = \case
  EndOfStack -> Nothing
  Elem !el !_st -> Just el
{-# INLINE top_ #-}

-- | Gets a single element from the given mutable stack if it is present.
top :: forall a m. PrimMonad m => Stack (PrimState m) a -> m (Maybe a)
top ref = readMutVar (getStackRef ref) >>= pure . top_
{-# INLINE top #-}

-- | Drops a single element from the given mutable stack.
-- If stack is empty, no changes will happen.
pop :: forall a m. PrimMonad m => Stack (PrimState m) a -> m ()
pop ref = readMutVar (getStackRef ref) >>= \case
  EndOfStack -> pure ()
  Elem !_el !st -> writeMutVar (getStackRef ref) st
{-# INLINE pop #-}

-- | Calculates size of the immutable stack.
size :: forall a. Stack_ a -> Int
size EndOfStack = 0
size (Elem !_a rest) =
  let !prevStackSize = size rest
  in succ prevStackSize
{-# INLINE size #-}
