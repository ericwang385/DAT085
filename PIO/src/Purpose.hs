{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Purpose (
    (:~:),
    Conj,
    All,
    Nil
) where
import Data.Data (Typeable)
import GHC.Exts (Constraint)
import If (type (||))

data All 
data Nil
data Conj :: * -> * -> *

type family Combine (a :: *) (b :: *) :: * where
    Combine a b = Conj a b

-- a is purpose of data while b is purpose of computation
-- data without any purpose can flow to any computation
-- computation with all purpose can receive any data
type family  (a :: *) :~: (b :: *) :: Constraint where
    a :~: All = a ~ a
    Nil :~: b = b ~ b
    (Conj a1 a2) :~: (Conj b1 b2) = ((a1 :~: b1) || (a1 :~: b2), (a2 :~: b1) || (a2 :~: b2))
    a :~: (Conj b1 b2) = (a :~: b1, a :~: b2)
    (Conj a1 a2) :~: b = (a1 :~: b) || (a2 :~: b)
    a :~: b = a ~ b