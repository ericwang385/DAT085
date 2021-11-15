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

type family NJoin (a :: Nat) (b :: Nat) :: Nat where
    NJoin Zero n = n
    NJoin (Suc m) Zero = Suc m
    NJoin (Suc m) (Suc n) = NJoin m n

data SNat :: Nat -> * where
  SZero :: SNat Zero
  SSuc  :: SNat a -> SNat (Suc a)

data Purpose = Nil | P Nat

--TODO: Conjuction
data Labled :: * -> Purpose -> * where
    LNil     :: a -> Labled a Nil
    LPurpose :: a -> Labled a p

type family (a :: Purpose) :-> (b :: Purpose) :: Bool where
    Nil     :-> b       = True
    P a     :-> Nil     = False
    P a     :-> P b     = (a := b)

type family Join (a :: Purpose) (b :: Purpose) :: Purpose where
    Join Nil n = n
    Join (P m) Nil = P m
    Join (P m) (P n) = P (NJoin m n)

testData :: Labled Int Nil
testData = LNil 1

testData2 :: Labled Int (P Zero)
testData2 = LPurpose 2

plus ::(((a :-> b) ~ True), (Join a b ~ c)) => Labled Int a -> Labled Int b -> Labled Int c
plus (LNil x) (LNil y) = LPurpose (x + y)
plus (LNil x) (LPurpose y) = LPurpose (x + y)
plus (LPurpose x) (LPurpose y) = LPurpose (x + y)
plus (LPurpose _) (LNil _) = undefined