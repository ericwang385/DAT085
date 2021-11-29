{-# LANGUAGE KindSignatures, DataKinds #-}

module Labled where
import Control.Applicative ()
import Purpose (Nil) 

newtype Labled p a = MkLabled a

instance Monad (Labled p) where
    return a = MkLabled a
    MkLabled a >>= k = MkLabled (let MkLabled b = k a in b)

instance Applicative (Labled p) where
    pure  = return 
    (<*>) = undefined

instance Functor (Labled p) where
    fmap = undefined

--Unsafe can only use internally
unLabled :: Labled l a -> a
unLabled (MkLabled a) = a

toNil :: Labled l a -> Labled Nil a
toNil (MkLabled a) = return a