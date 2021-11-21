module PIO where

import Labled (Labled, unLabled)
import GHC.Base (Applicative)

newtype PIO l a = MkPIO (IO (Labled l a))

instance Monad (PIO l) where
    return a = MkPIO $ return $ return a

    MkPIO m >>= k = MkPIO $ do
            pa <- m 
            let MkPIO m' = k (unLabled pa)
            m'

instance Applicative (PIO l) where
    pure  = undefined 
    (<*>) = undefined

instance Functor (PIO l) where
    fmap = undefined
