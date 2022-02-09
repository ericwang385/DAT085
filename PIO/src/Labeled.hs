{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE CPP #-}

module Labeled where

import Effect
import Purpose
import Data.Kind (Type)

newtype Labeled p a = MkLabeled { val :: a }

instance Effect Labeled where
  type CanFlowTo Labeled a b = Purpose.CanFlowTo a b
  type Btm Labeled = All
  type Join Labeled a b = Combine a b

  return = MkLabeled
  (MkLabeled a) >>= f = MkLabeled . val $ f a
  
tag :: a -> Type -> Labeled Type a 
tag x _ = MkLabeled x