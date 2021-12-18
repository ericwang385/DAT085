{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Ref where

import Data.IORef (IORef)
import PIO
import Labeled
import Purpose

newtype Ref (p :: Purpose) a = MkRef (IORef a)


createRef :: IORef a -> Ref p a
createRef = MkRef

newRefPIO :: a -> PIO p (Ref p' a)
newRefPIO = undefined

readRefPIO :: Ref p a -> PIO p' (Labeled p a)
readRefPIO = undefined

writeRefPIO :: Ref p a -> a -> PIO All ()
writeRefPIO = undefined