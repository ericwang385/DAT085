{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE CPP #-}

module Labeled where

import Purpose
import Control.Effect
import Prelude hiding (Monad(..))
import Data.Type.Bool
import Data.Kind (Type)

-- The `T` monad family from the DCC paper
newtype Labeled (l :: Purpose) a = MkLabeled a deriving(Eq, Show)

instance Effect Labeled where
  type Plus Labeled Register Ads = Nil
  type Plus Labeled Ads Register = Nil
  type Plus Labeled Register Register = Register
  type Plus Labeled Ads Ads = Ads
  type Plus Labeled Nil Nil = Nil
  type Plus Labeled All All = All
  type Inv Labeled a b = (a :< b) ~ True
  type Unit Labeled = All

  return = MkLabeled
  (MkLabeled a) >>= f = f a

--type family CanFlowTo (l :: Purpose) (t :: Type) where
--  CanFlowTo l (Labeled l' a) = l :< l'
--  CanFlowTo l ()             = True
--  CanFlowTo l (s, t)         = CanFlowTo l s && CanFlowTo l t
--  CanFlowTo l (s -> t)       = CanFlowTo l t
--  CanFlowTo l _              = False
--
--infixl >>>=
--(>>>=) :: (l `CanFlowTo` s) ~ True => Labeled l a -> (a -> s) -> s
--(MkLabeled a) >>>= f = f a

--instance Monad (Labeled l) where
--  return = MkLabeled
--  (>>=)  = (>>>=)

--instance Applicative (Labeled l) where
--  pure  = return
--  (<*>) = ap
--
--instance Functor (Labeled l) where
--  fmap = liftM

tag :: a -> Purpose -> Labeled p a
tag a _ = MkLabeled a

up :: (Less p p') => Labeled p a -> Labeled p' a
up (MkLabeled a) = MkLabeled a

down :: (Less p' p) => Labeled p a -> Labeled p' a
down (MkLabeled a) = MkLabeled a

dup :: Labeled (Purpose.Join p p') a -> Labeled p (Labeled p' a)
dup (MkLabeled a) = MkLabeled (MkLabeled a)

join :: Labeled p (Labeled p' a) -> Labeled (Purpose.Join p p') a
join (MkLabeled (MkLabeled a)) = MkLabeled a

com :: Labeled p (Labeled p' a) -> Labeled p' (Labeled p a)
com (MkLabeled (MkLabeled a)) = MkLabeled (MkLabeled a)

extract :: Labeled p a -> a
extract (MkLabeled a) = a
  
unLabeled :: Labeled All a -> a
unLabeled (MkLabeled a) = a

hash :: Labeled p String -> Labeled All String
hash = return . extract