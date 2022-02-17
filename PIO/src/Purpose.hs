{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

{-# LANGUAGE UndecidableInstances #-}

module Purpose where

import Data.Kind (Constraint)
import Data.Type.Set
import GHC.TypeLits
import Prelude hiding (Monad(..))

data All

--type instance Cmp a a = 'EQ
type instance Cmp a b = CmpSymbol a b

type family Join a b where
  Join (Set a) (Set b) = Set(Union a b)
  Join a (Set b) = Set(Union '[a] b)

type family CanFlowTo a b :: Constraint where
  CanFlowTo All a = a ~ a
  CanFlowTo (Set (a ': b)) (Set (a ': c)) = CanFlowTo (Set b) (Set c)
  CanFlowTo (Set (a ': b)) (Set (c ': d)) = CanFlowTo (Set b) (Set (c ': d))
  CanFlowTo (Set (a ': b)) a = a ~ a
  CanFlowTo (Set (a ': b)) c = CanFlowTo (Set b) c
  CanFlowTo a (Set '[]) = a ~ a
  --CanFlowTo a (Set (a ': c)) = a ~ a
  --CanFlowTo a (Set (b ': c)) = CanFlowTo a (Set c)

--  CanFlowTo (Set (a1 ': a2)) (Set (a1 ': b)) = CanFlowTo a2 b
--  CanFlowTo (Purpose a1 a2) (Purpose a2 b) = CanFlowTo a1 b
--  CanFlowTo a1 (Purpose a1 a2) = a1 ~ a1
--  CanFlowTo a2 (Purpose a1 a2) = a1 ~ a1
--  CanFlowTo (Purpose a b) c = (CanFlowTo a c, CanFlowTo b c)
--  CanFlowTo a b = a ~ b