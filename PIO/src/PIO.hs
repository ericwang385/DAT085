module PIO where

import Labled (Labled, unLabled)
import GHC.Base (Applicative)

newtype PIO l a = MkPIO (IO (Labled l a))

-- Shall only be Monad not Functor for the defination of purpose limitation
instance Monad (PIO l) where
    return a = MkPIO $ return $ return a

    MkPIO m >>= k = MkPIO $ do
            pa <- m 
            let MkPIO m' = k (unLabled pa)
            m'

-- Appeals simply to meet Haskell demand
instance Applicative (PIO l) where
    pure  = return 
    (<*>) = undefined

instance Functor (PIO l) where
    fmap = undefined
