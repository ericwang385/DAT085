{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Ref where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import PIO
import Labeled
import Purpose

newtype Ref (p :: Purpose) a = MkRef (IORef a)

createRef :: IORef a -> Ref p a
createRef = MkRef

newRefPIO :: a -> PIO p (Ref p' a)
newRefPIO ref = do
          ref' <- iotoPIO $ newIORef ref
          return $ MkRef ref'

readRefPIO :: Ref p a -> PIO p' (Labeled p a)
readRefPIO (MkRef ref) = do
          ref' <- iotoPIO $ readIORef ref
          return $ return ref'

writeRefPIO :: Ref p a -> a -> PIO All ()
writeRefPIO (MkRef ref) a = iotoPIO $ writeIORef ref a