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

register :: Labeled (Set '[]) Bool
register = verifyIP userIP >>=
           \ans1 -> if not ans1 then tag False
                   else userExist username >>=
           \ans2 -> if ans2 then tag False
                   else updateDB username usermail password >>=
           \ans3 -> if not ans3 then tag False
                   else sendmail usermail >>=
           \ans4 -> if ans4 then tag True :: Labeled (Set '[]) Bool else (up . return) False


login :: Labeled (Set '[]) Bool
login = verifyIP userIP >>=
        \ans1 -> if not ans1 then tag False
                 else checkPass username password >>=
        \ans2 -> if ans2 then tag True :: Labeled (Set '[]) Bool  else tag False :: Labeled (Set '[]) Bool

checkPass :: Labeled (Set Register) String -> Labeled (Set Login) String -> Labeled (Set Verify) Bool
checkPass _ pass = password >>= \p -> pass >>= \p' -> if p == p' then tag True :: Labeled (Set Verify) Bool else tag False :: Labeled (Set Verify) Bool

sendmail :: Labeled p String -> Labeled (Set SendMail) Bool
sendmail _ = tag True

userExist :: Labeled p String -> Labeled (Set Verify) Bool
userExist name = searchDB name >>= \ans -> if ans then tag True :: Labeled (Set Verify) Bool else tag False :: Labeled (Set Verify) Bool

verifyIP :: Labeled p String -> Labeled (Set Verify) Bool
verifyIP _ = tag True

searchDB :: Labeled p String -> Labeled (Set Login) Bool
searchDB _ = tag True

updateDB :: Labeled p1 String -> Labeled p2 String -> Labeled p3 String -> Labeled (Set WriteDB) Bool
updateDB _ _ _ = tag True


