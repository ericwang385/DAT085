{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module Labeled where

import Control.Effect
import Purpose
import Prelude hiding (Monad(..))

newtype Labeled p a = MkLabeled { val :: a }

instance Effect Labeled where
  type Unit Labeled = All
  type Inv Labeled a b = CanFlowTo a b
  type Plus Labeled a b = Join a b

  return = MkLabeled
  (MkLabeled a) >>= f = MkLabeled . val $ f a

up :: (CanFlowTo p1 p2) => Labeled p1 a -> Labeled p2 a
up (MkLabeled a) = MkLabeled a

tag :: a -> Labeled p a
tag = MkLabeled