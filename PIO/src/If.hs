{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module If (
    type(||),
    type(&&),
    TAnd,
    TOr
)
where
import Data.Kind (Constraint)

type (||) :: Constraint -> Constraint -> Constraint
type family a || b where
    a || b = TOr (IsSat a) (IsSat b) ~ True

type (&&) :: Constraint -> Constraint -> Bool
type family a && b :: Bool where
    a && b = TAnd (IsSat a) (IsSat b)

type IsSat :: Constraint -> Bool
type family IsSat ct where

type TOr :: Bool -> Bool -> Bool
type family TOr a b where
    TOr False False = False
    TOr False True = True
    TOr True b = True

type TAnd :: Bool -> Bool -> Bool
type family TAnd a b where
    TAnd True True = True
    TAnd a b = False