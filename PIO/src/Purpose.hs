{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Purpose where

data Purpose = All | Nil | Register | Ads deriving(Eq, Show)

class Flows (l1 :: Purpose) (l2 :: Purpose)
instance Flows All All
instance Flows Nil Nil
instance Flows Register Register
instance Flows Ads Ads
instance Flows All Register
instance Flows All Ads
instance Flows All Nil
instance Flows Register Nil
instance Flows Ads Nil

class Less (l1 :: Purpose) (l2 :: Purpose)
instance Less Register Nil
instance Less Ads Nil
instance Less All Nil
instance Less All Register
instance Less All Ads

type family Join (l1 :: Purpose) (l2 :: Purpose) :: Purpose where
    Join Register Ads = Nil
    Join Ads Register = Nil
    Join Register Register = Register
    Join Ads Ads = Ads
    Join Nil Nil = Nil
    Join All All = All
