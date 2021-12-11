{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module Labeled (
    Labeled,
    extract,
    up
) where
import Control.Applicative ()
import Purpose (Purpose, Flows, Join)
import Control.Monad (ap)


newtype Labeled (p :: Purpose) a = MkLabled a deriving(Eq, Show)

instance Monad (Labeled p) where
    return a = MkLabled a
    MkLabled a >>= k = MkLabled (let MkLabled b = k a in b)

instance Applicative (Labeled p) where
    pure  = return 
    (<*>) = ap

instance Functor (Labeled p) where
    fmap f (MkLabled a) = MkLabled $ f a

up :: (Flows p p') => Labeled p a -> Labeled p' a
up (MkLabled a) = MkLabled a 

dup :: Labeled (Join l l') a -> Labeled l (Labeled l' a)
dup (MkLabled a) = MkLabled (MkLabled a)

join :: Labeled l (Labeled l' a) -> Labeled (Join l l') a 
join (MkLabled (MkLabled a)) = MkLabled a

com :: Labeled l (Labeled l' a) -> Labeled l' (Labeled l a)
com (MkLabled (MkLabled a)) = MkLabled (MkLabled a)

extract :: Labeled l a -> a
extract (MkLabled a) = a