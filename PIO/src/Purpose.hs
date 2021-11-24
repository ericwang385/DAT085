{-# LANGUAGE GADTs, DataKinds, TypeFamilies, TypeOperators, RankNTypes #-}
module Purpose (
    Purpose,
    (:~:),
    Higher,
    Lower,
    BasicPurpose
) where
import Data.Data (Typeable)
import GHC.Exts (Constraint)

-- Zero stands for no limit in purpose
-- All stands for all kinds of purpose
data Nat = Zero | Suc Nat
    deriving(Typeable)

data Purpose = All | P Nat
    deriving(Typeable)

-- a is purpose of data while b is purpose of computation
type family  (a :: Purpose) :~: (b :: Purpose) :: Constraint where
    a :~: All = a ~ a
    P Zero :~: b = b ~ b
    P (Suc a) :~: P Zero = a ~ a
    P (Suc a) :~: P (Suc b) = P a :~: P b

type family Higher (a :: Purpose) :: Purpose where
    Higher (P (Suc n)) = P (Suc (Suc n))

type family Lower (a :: Purpose) :: Purpose where
    Lower (P (Suc n)) = P n

type BasicPurpose = P (Suc Zero)
