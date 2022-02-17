{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module TypeChecked where

import Labeled
import Prelude hiding (Monad(..))
import Effect
import Data.Type.Set
import Data.Kind (Type)

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

register :: Labeled (Set '[]) Bool
register = verifyIP userIP >>=
           \ans1 -> if not ans1 then tag False
                   else userExist username >>=
           \ans2 -> if ans2 then tag False
                   else updateDB username usermail password >>=
           \ans3 -> if not ans3 then tag False
                   else sendmail usermail >>=
           \ans4 -> if ans4 then tag True else tag False


login :: Labeled (Set '[]) Bool
login = verifyIP userIP >>=
        \ans1 -> if not ans1 then tag False
                 else checkPass username password >>=
        \ans2 -> if ans2 then tag True else tag False

checkPass :: Labeled Register String -> Labeled Login String -> Labeled Verify Bool
checkPass name pass = password >>= \p -> pass >>= \p' -> if p == p' then tag True else tag False

sendmail :: Labeled p String -> Labeled SendMail Bool
sendmail _ = tag True

userExist :: Labeled p String -> Labeled Verify Bool
userExist name = searchDB name >>= \ans -> if ans then tag True else tag False

verifyIP :: Labeled p String -> Labeled Verify Bool
verifyIP _ = tag True

searchDB :: Labeled p String -> Labeled Login Bool
searchDB _ = tag True

updateDB :: Labeled p1 String -> Labeled p2 String -> Labeled p3 String -> Labeled WriteDB Bool
updateDB _ _ _ = tag True


