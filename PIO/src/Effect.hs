{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds, ConstraintKinds, MultiParamTypeClasses #-}

module Effect where

import Data.Kind ( Constraint, Type)
import Prelude hiding (Monad(..))


class Effect (m :: p -> Type -> Type) where
  type CanFlowTo m (f :: p) (g :: p) :: Constraint
  type Join m (f :: p) (g :: p) :: p
  type Btm m :: p

  return :: a -> m (Btm m) a
  (>>=) :: (CanFlowTo m f g) => m f a -> (a -> m g b) -> m g b
  (>>) :: (CanFlowTo m f g) => m f a -> m g b -> m g b
  x >> y = x >>= (\_ -> y)
