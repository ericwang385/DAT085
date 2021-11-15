{-# LANGUAGE DataKinds, TypeFamilies, GADTs, TypeOperators, RankNTypes #-}
module Label where

import Prelude
import Data.Eq (Eq)


data Nat = Zero | Suc Nat

type family (a :: Nat) := (b :: Nat) :: Bool where
    Zero    := Zero    = True
    Zero    := (Suc n) = False
    (Suc m) := Zero    = False
    (Suc m) := (Suc n) = m := n

data SNat :: Nat -> * where
  SZero :: SNat Zero
  SSuc  :: SNat a -> SNat (Suc a)

data Purpose = Nil | Purpose Nat

--TODO: Conjuction
data Labled :: * -> Purpose -> * where
    LNil     :: Labled a Nil
    LPurpose :: Labled a p

type family (a :: Labled) :-> (b :: Labled) :: Bool where
    LNil        :-> b           = True
    LPurpose p  :-> LNil        = False
    LPurpose p1 :-> LPurpose p2 = (p1 := p2)