{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module TypeChecked where

import Labeled
import Prelude hiding (Monad(..))
import Control.Effect
import Data.Type.Set
import Purpose (Natural, All, PUnion)

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
register = userExist username >>=
           \ans2 -> if ans2 then tag False
                   else updateDB database username password >> sendmail usermail >>=
           \ans4 -> if ans4 then tag True :: Labeled (Set '[]) Bool else (up . return) False

login :: Labeled (Set '[]) Bool
login = verifyIP userIP >>=
        \ans1 -> if not ans1 then tag False
                 else checkPass username password >>=
        \ans2 -> if ans2 then tag True :: Labeled (Set '[]) Bool  else tag False :: Labeled (Set '[]) Bool

checkPass :: Labeled Register String -> Labeled Login String -> Labeled Verify Bool
checkPass _ pass = password >>= \p -> pass >>= \p' -> if p == p' then tag True :: Labeled Verify Bool else tag False :: Labeled Verify Bool

sendmail :: Labeled Login String -> Labeled SendMail Bool
sendmail ma = ma >>= \a -> tag True :: Labeled SendMail Bool

userExist :: Labeled Register String -> Labeled Verify Bool
userExist name = searchDB name >>= \ans -> if ans then tag True :: Labeled Verify Bool else tag False :: Labeled Verify Bool

verifyIP :: Labeled Verify String -> Labeled Verify Bool
verifyIP ma = ma >>= \a -> tag True :: Labeled Verify Bool

searchDB :: Labeled Register String -> Labeled Login Bool
searchDB _ = tag True :: Labeled Login Bool

updateDB :: DB -> Labeled Register String -> Labeled Login String -> DB
updateDB db name pass = db >>= \rawdb -> tag (rawdb ++ [(name, pass)]) :: DB


