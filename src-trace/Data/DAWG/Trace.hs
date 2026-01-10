module Data.DAWG.Trace where

import Control.Monad.Primitive (PrimMonad)
import System.IO.Unsafe
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.Vector.Hashtables as HT

traceIO :: PrimMonad m => String -> m ()
traceIO str = HT.unsafeIOToPrim . putStrLn $ str

traceWith :: forall a b m. PrimMonad m => (b -> IO ()) -> a -> m ()
traceWith action a = (HT.unsafeIOToPrim . action . unsafeCoerce) a

tracePure :: String -> a -> a
tracePure str a = unsafePerformIO do
  traceIO str
  pure $! a
