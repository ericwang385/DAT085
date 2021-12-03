{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Purpose where




data Purpose = All | Nil | Register | Ads

class Flows (l1 :: Purpose) (l2 :: Purpose)
instance Flows All All 
instance Flows Nil Nil 
instance Flows Register Register
instance Flows Ads Ads 
instance Flows Nil Register
instance Flows Nil Ads
instance Flows Nil All 
instance Flows Register All
instance Flows Ads Register
instance Flows Ads All 

type family Join (l1 :: Purpose) (l2 :: Purpose) :: Purpose where
    Join Register Ads = All
    Join Ads Register = All