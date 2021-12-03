{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module Labled where
import Control.Applicative ()
import Purpose (Purpose)
import Control.Monad (ap)


newtype Labled (p :: Purpose) a = MkLabled a

instance Monad (Labled p) where
    return a = MkLabled a
    MkLabled a >>= k = MkLabled (let MkLabled b = k a in b)

instance Applicative (Labled p) where
    pure  = return 
    (<*>) = ap

instance Functor (Labled p) where
    fmap f (MkLabled a) = MkLabled $ f a

--Unsafe can only use internally
unLabled :: Labled l a -> a
unLabled (MkLabled a) = a