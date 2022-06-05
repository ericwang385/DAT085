{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module TypeChecked where

import Labeled
import Prelude hiding (Monad(..))
import Control.Effect
import Data.Type.Set
import Purpose (Natural, PUnion)

type SendMail = Set '[Natural 1]
type ReadDB = Set '[Natural 2]
type WriteDB = Set '[Natural 3]
type Verify = Set '[Natural 4]


type Login = PUnion ReadDB (PUnion SendMail Verify)
type Marketing = PUnion ReadDB SendMail
type Register = PUnion ReadDB (PUnion WriteDB (PUnion SendMail Verify))

username :: Labeled Register String
username = tag "TestName1"

usermail :: Labeled Login String
usermail = tag "Testmail1"

userIP :: Labeled Verify String
userIP = tag "TestIP"

password :: Labeled Login String
password = tag "TestPassword"

sendAds :: Labeled Marketing Bool
sendAds = usermail >>= \mail -> userIP >>= \ip -> tag True :: Labeled Marketing Bool



