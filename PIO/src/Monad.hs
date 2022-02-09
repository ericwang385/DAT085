{-# LANGUAGE TypeFamilies, GADTs #-}

module Monad where

import Effect
import Prelude hiding (Monad(..))
import qualified Prelude as P

data Monad m f a where
    Wrap :: P.Monad m => m a -> Monad m () a

{-| Unwrap a monad -}
unWrap :: P.Monad m => Monad m f a -> m a
unWrap (Wrap m) = m

instance (P.Monad m) => Effect (Monad m) where
    {-| Trivial singleton monoid -}
    type CanFlowTo (Monad m) s t    = ()
    type Btm (Monad m)       = ()
    type Join (Monad m) s t   = ()

    return x = Wrap (P.return x)
    (Wrap x) >>= f = Wrap ((P.>>=) x (unWrap . f))