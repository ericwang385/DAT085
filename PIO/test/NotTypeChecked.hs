{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module TypeChecked where

import Labeled
import Prelude hiding (Monad(..))
import Effect
import Data.Type.Set


data SendMail
data ReadDB
data WriteDB
data Verify


type Login = Set '[ReadDB, Verify]
type Marketing = Set '[ReadDB, SendMail]
type Register = Set '[ReadDB, WriteDB, SendMail, Verify]

username = tag "TestName1" :: Labeled Register String
usermail = tag "Testmail1" :: Labeled Login String
userIP = tag "TestIP" :: Labeled Verify String
password = tag "TestPassword" :: Labeled Login String

sendAds :: Labeled Marketing Bool
sendAds = usermail >>= \mail -> userIP >>= \ip -> tag True



