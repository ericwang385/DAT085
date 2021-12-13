{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Labeled where

import Purpose (Purpose(All), Flows, Join, Less)
import Control.Monad (ap)


newtype Labeled (p :: Purpose) a = MkLabeled a deriving(Eq, Show)

instance Monad (Labeled p) where
    return a = MkLabeled a
    MkLabeled a >>= k = MkLabeled (let MkLabeled b = k a in b)

instance Applicative (Labeled p) where
    pure  = return
    (<*>) = ap

instance Functor (Labeled p) where
    fmap f (MkLabeled a) = MkLabeled $ f a

up :: (Less p p') => Labeled p a -> Labeled p' a
up (MkLabeled a) = MkLabeled a

down :: (Less p' p) => Labeled p a -> Labeled p' a
down (MkLabeled a) = MkLabeled a

dup :: Labeled (Join p p') a -> Labeled p (Labeled p' a)
dup (MkLabeled a) = MkLabeled (MkLabeled a)

join :: Labeled p (Labeled p' a) -> Labeled (Join p p') a
join (MkLabeled (MkLabeled a)) = MkLabeled a

com :: Labeled p (Labeled p' a) -> Labeled p' (Labeled p a)
com (MkLabeled (MkLabeled a)) = MkLabeled (MkLabeled a)

extract :: Labeled p a -> a
extract (MkLabeled a) = a

unLabeled :: Labeled All a -> a
unLabeled (MkLabeled a) = a