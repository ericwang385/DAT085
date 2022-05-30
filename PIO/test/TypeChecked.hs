{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module TypeChecked where

import Labeled
import Prelude hiding (Monad(..))
import Control.Effect
import Data.Type.Set
import Purpose (Natural, All)

type SendMail = '[Natural 1]
type ReadDB = '[Natural 2]
type WriteDB = '[Natural 3]
type Verify = '[Natural 4]


type Login = Union ReadDB (Union SendMail Verify)
type Marketing = Union ReadDB SendMail
type Register = Union ReadDB (Union WriteDB (Union SendMail Verify))
type DB = Labeled All [(Labeled (Set Register) String, Labeled (Set Login) String)]

username :: Labeled (Set Register) String
username = tag "TestName1"

usermail :: Labeled (Set Login) String
usermail = tag "Testmail1"

userIP :: Labeled (Set Verify) String
userIP = tag "TestIP"

password :: Labeled (Set Login) String
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

checkPass :: Labeled (Set Register) String -> Labeled (Set Login) String -> Labeled (Set Verify) Bool
checkPass _ pass = password >>= \p -> pass >>= \p' -> if p == p' then tag True :: Labeled (Set Verify) Bool else tag False :: Labeled (Set Verify) Bool

sendmail :: Labeled (Set Login) String -> Labeled (Set SendMail) Bool
sendmail ma = ma >>= \a -> tag True :: Labeled (Set SendMail) Bool

userExist :: Labeled (Set Register) String -> Labeled (Set Verify) Bool
userExist name = searchDB name >>= \ans -> if ans then tag True :: Labeled (Set Verify) Bool else tag False :: Labeled (Set Verify) Bool

verifyIP :: Labeled (Set Verify) String -> Labeled (Set Verify) Bool
verifyIP ma = ma >>= \a -> tag True :: Labeled (Set Verify) Bool

searchDB :: Labeled (Set Register) String -> Labeled (Set Login) Bool
searchDB _ = tag True :: Labeled (Set Login) Bool

updateDB :: DB -> Labeled (Set Register) String -> Labeled (Set Login) String -> DB
updateDB db name pass = db >>= \rawdb -> tag (rawdb ++ [(name, pass)]) :: DB


