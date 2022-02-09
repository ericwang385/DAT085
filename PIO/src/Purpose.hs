{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Purpose where

import Data.Kind (Constraint)


data All
data P
data Nil
data Conj a b

type family Combine a b where
  Combine a All = a
  Combine All a = a
  Combine a a = a
  Combine a (Conj b c) = Conj a (Conj b c)

type family CanFlowTo a b :: Constraint where
  CanFlowTo All a = a ~ a
  CanFlowTo a Nil = a ~ a
  CanFlowTo (Conj a1 a2) (Conj a1 b) = CanFlowTo a2 b
  CanFlowTo (Conj a1 a2) (Conj a2 b) = CanFlowTo a1 b
  CanFlowTo a1 (Conj a1 a2) = a1 ~ a1
  CanFlowTo a2 (Conj a1 a2) = a1 ~ a1
  CanFlowTo (Conj a b) c = (CanFlowTo a c, CanFlowTo b c)
  CanFlowTo a b = a ~ b


