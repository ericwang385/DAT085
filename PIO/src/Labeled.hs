{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module Labeled where

import Effect
import Purpose
import Prelude hiding (Monad(..))

newtype Labeled p a = MkLabeled { val :: a }

instance Effect Labeled where
  type Btm Labeled = All
  type CanFlowTo Labeled a b = Purpose.CanFlowTo a b
  type Join Labeled a b = Purpose.Join a b

  return = MkLabeled
  (MkLabeled a) >>= f = MkLabeled . val $ f a

up :: (Purpose.CanFlowTo p1 p2) => Labeled p1 a -> Labeled p2 a
up (MkLabeled a) = MkLabeled a

tag :: a -> Labeled p a
tag = MkLabeled