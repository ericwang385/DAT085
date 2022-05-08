{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

{-# LANGUAGE UndecidableInstances #-}

module Purpose where

import Data.Kind (Constraint)
import Data.Type.Set
import GHC.TypeLits ( Nat, CmpNat, type (+) )
import Prelude hiding (Monad(..))

type All = Set '[Natural 0]

data Natural (a :: Nat) where
    Z :: Natural 0
    S :: Natural n -> Natural (n + 1)

type instance Cmp (Natural n) (Natural m) = CmpNat n m
--type instance Cmp a b = CmpSymbol a b

type Intersection s t = Dup (Sort (s :++ t))

type family Dup t where
    Dup '[]           = '[]
    Dup '[e]          = '[]
    Dup (e ': e ': s) = e ': Dup s
    Dup (e ': f ': s) = Dup (f ': s)

type family Join a b where
  Join All (Set b) = Set b
  Join (Set a) (Set b) = Set (Intersection a b)

type family CanFlowTo a b :: Constraint where
  CanFlowTo All a = a ~ a
  CanFlowTo (Set a) (Set b) = Union a b ~ a