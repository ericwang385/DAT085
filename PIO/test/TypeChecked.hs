{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RebindableSyntax #-}

module TypeChecked where

import Labeled
import Prelude hiding (Monad(..))
import Control.Effect

import Data.Type.Set
import Purpose (Natural, All, PUnion)
import Control.Effect.Parameterised.ExtensibleState (ifThenElse)

type SendMail = Set '[Natural 1]
type ReadDB = Set '[Natural 2]
type WriteDB = Set '[Natural 3]
type Verify = Set '[Natural 4]


type Login = PUnion ReadDB (PUnion SendMail Verify)
type Marketing = PUnion ReadDB SendMail
type Register = PUnion ReadDB (PUnion WriteDB (PUnion SendMail Verify))
type DB = Labeled All [(Labeled Register String, Labeled Login String)]

username :: Labeled Register String
username = tag "TestName1"

usermail :: Labeled Login String
usermail = tag "Testmail1"

userIP :: Labeled Verify String
userIP = tag "TestIP"

password :: Labeled Login String
password = tag "TestPassword"

database :: DB
database = tag [(username, password)]

register :: Labeled (Set '[]) Bool
register = do
            ans <- userExist username
            if ans then tag False :: Labeled (Set '[]) Bool
            else do
                _ <- updateDB database username password
                ans3 <- sendmail usermail
                if ans3 then tag True :: Labeled (Set '[]) Bool else tag False :: Labeled (Set '[]) Bool

login :: Labeled (Set '[]) Bool
login = do
         ans1 <- verifyIP userIP
         if not ans1 then tag False
         else do
            ans2 <- checkPass username password
            if ans2 then tag True :: Labeled (Set '[]) Bool  else tag False :: Labeled (Set '[]) Bool

checkPass :: Labeled Register String -> Labeled Login String -> Labeled Verify Bool
checkPass _ pass = do
                p <- password
                p' <- pass
                if p == p' then tag True :: Labeled Verify Bool else tag False :: Labeled Verify Bool

sendmail :: Labeled Login String -> Labeled SendMail Bool
sendmail ma = do
            _ <- ma
            tag True :: Labeled SendMail Bool

userExist :: Labeled Register String -> Labeled Verify Bool
userExist name = do
                ans <- searchDB name
                if ans then tag True :: Labeled Verify Bool else tag False :: Labeled Verify Bool

verifyIP :: Labeled Verify String -> Labeled Verify Bool
verifyIP ma = ma >>= \a -> tag True :: Labeled Verify Bool

searchDB :: Labeled Register String -> Labeled Login Bool
searchDB _ = tag True :: Labeled Login Bool

updateDB :: DB -> Labeled Register String -> Labeled Login String -> DB
updateDB db name pass = do
                    rawdb <- db
                    tag (rawdb ++ [(name, pass)]) :: DB


