module PIO where

import Labeled (Labeled, extract)
import Control.Monad (ap)

newtype PIO l a = MkPIO (IO (Labeled l a))

-- -- Shall only be Monad not Functor for the definition of purpose limitation
instance Monad (PIO l) where
    return a = MkPIO $ return $ return a

    MkPIO m >>= k = MkPIO $ do
            pa <- m
            let MkPIO m' = k (extract pa)
            m'

-- Appeals simply to meet Haskell demand
instance Applicative (PIO l) where
    pure  = return
    (<*>) = ap

instance Functor (PIO l) where
    fmap f (MkPIO a) = MkPIO $ fmap f <$> a

-- toPIO :: IO a -> PIO l a
-- toPIO io = MkPIO $ io <&> return