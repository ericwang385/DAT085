{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}

module PIO where

import Labeled (Labeled, extract)
import Data.Functor
import Control.Monad
import Purpose

newtype PIO p a = MkPIO (IO (Labeled p a))


instance Monad (PIO p) where
    return a = MkPIO $ return $ return a

    MkPIO m >>= k = MkPIO $ do
            pa <- m
            let MkPIO m' = k (extract pa)
            m'

instance Applicative (PIO p) where
    pure  = return
    (<*>) = ap

instance Functor (PIO p) where
    fmap = liftM

addLabel :: forall p p' a . Flows p p' => PIO p' a -> PIO p (Labeled p' a)
addLabel (MkPIO m) = MkPIO $ return <$> m

toPIO :: Labeled p a -> PIO p a
toPIO a = MkPIO (return a)

iotoPIO :: IO a -> PIO p a
iotoPIO io = MkPIO $ io <&> return

run :: PIO p a -> IO (Labeled p a)
run (MkPIO m) = m

