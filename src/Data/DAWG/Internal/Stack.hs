module Data.DAWG.Internal.Stack where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Foldable
import Data.Primitive.MutVar

-- ** Stack

newtype Stack m a = StackRef { unStackRef :: MutVar m (Stack_ a) }

data Stack_ a
  = Elem !a !(Stack_ a)
  | EndOfStack
  deriving Foldable

instance Show a => Show (Stack_ a) where
  show = show . reverse . toList

push :: forall a m. PrimMonad m => a -> Stack (PrimState m) a -> m ()
push v ref =
  let go = \case
        EndOfStack -> Elem v EndOfStack
        Elem !prev !st -> Elem v (Elem prev st)
  in modifyMutVar' (unStackRef ref) go

top_ :: Stack_ a -> Maybe a
top_ = \case
  EndOfStack -> Nothing
  Elem !el !_st -> Just el

top :: forall a m. PrimMonad m => Stack (PrimState m) a -> m (Maybe a)
top ref = readMutVar (unStackRef ref) >>= pure . top_

pop :: forall a m. PrimMonad m => Stack (PrimState m) a -> m ()
pop ref = readMutVar (unStackRef ref) >>= \case
  EndOfStack -> pure ()
  Elem !_el !st -> writeMutVar (unStackRef ref) st

size :: forall a. Stack_ a -> Int
size EndOfStack = 0
size (Elem !_a rest) =
  let !prevStackSize = size rest
  in 1 + prevStackSize
