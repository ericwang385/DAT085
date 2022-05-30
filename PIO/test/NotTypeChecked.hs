{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module TypeChecked where

import Labeled
import Prelude hiding (Monad(..))
import Control.Effect
import Data.Type.Set
import Purpose (Natural)

type SendMail = '[Natural 1]
type ReadDB = '[Natural 2]
type WriteDB = '[Natural 3]
type Verify = '[Natural 4]


type Login = Union ReadDB Verify
type Marketing = Union ReadDB SendMail
type Register = Union ReadDB (Union WriteDB (Union SendMail Verify))

username :: Labeled (Set Register) String
username = tag "TestName1"

usermail :: Labeled (Set Login) String
usermail = tag "Testmail1"

userIP :: Labeled (Set Verify) String
userIP = tag "TestIP"

password :: Labeled (Set Login) String
password = tag "TestPassword"

sendAds :: Labeled (Set Marketing) Bool
sendAds = usermail >>= \mail -> userIP >>= \ip -> tag True :: Labeled (Set Marketing) Bool



