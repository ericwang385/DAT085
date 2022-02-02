{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Effect where

import Data.Kind ( Constraint, Type)
import Prelude hiding (Monad(..))


class Effect (m :: p -> Type -> Type) where
  type CanFlowTo (f :: p) (g :: p) :: Constraint
  type Join (f :: p) (g :: p) :: p
  type Btm :: p

  return :: a -> m Btm a
  (>>=) :: (CanFlowTo f g) => m f a -> (a -> m g b) -> m g b
