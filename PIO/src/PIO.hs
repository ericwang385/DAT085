module PIO (
    PIO,
    nilLabled,
    run
)
where

import Labled (Labled, unLabled, toNil)
import GHC.Base (Applicative)
import Purpose ( Nil )

newtype PIO l a = MkPIO (IO (Labled l a))

-- Shall only be Monad not Functor for the defination of purpose limitation
instance Monad (PIO l) where
    return a = MkPIO (return (return a))

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

run :: PIO l a -> IO (Labled l a)
run (MkPIO m) = m

nilLabled :: PIO l a -> PIO Nil a
nilLabled (MkPIO m) = MkPIO $ do
            pa <- m
            let m' = toNil pa
            return m'