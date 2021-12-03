module Ref where

import Data.IORef
import PIO (PIO, toPIO)
import Labled (Labled)
import Purpose

newtype Ref l a = MkRef (IORef a)

toRef :: IORef a -> Ref l a
toRef = MkRef

newRefPIO :: a -> PIO p (Ref p' a)
newRefPIO a = do 
                ref <- toPIO $ newIORef a
                return (MkRef ref)

readRefSecIO :: Ref Purpose a -> PIO p' (Labled p a)
readRefSecIO (MkRef ref) = do a <- toPIO $ readIORef ref
                              return (return a)

writeRefSecIO :: Ref Purpose a -> a -> PIO p ()
writeRefSecIO (MkRef ref) a = toPIO $ writeIORef ref a