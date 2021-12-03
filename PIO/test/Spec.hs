{-# LANGUAGE TypeOperators #-}

import Data.List
import Data.Char
import Data.Maybe
import Labled ( Labled, unLabled, toNil )
import PIO ( PIO, run, nilLabled)
import Purpose ( (:~:), Conj, All, Nil )
import Data.Hashable


main :: IO ()
main = test

data Register
data Ads

type DB = [(Labled * String, Labled * String)]

test :: IO ()
test = do   
        putStr "Input user name:"
        name <- getLine 
        -- user name can only be used in register
        let sname = return name :: Labled Register String
        let db = return [] :: Labled Nil DB
        -- hashed name can be used in marketing
        let hname = hashName sname
        putStr "Input user email"
        mail <- getLine
        let smail = return mail :: Labled (Conj Register Ads) String
        r <- run $ regist db sname smail
        if unLabled r then putStrLn "Register Success!" else putStrLn "Register Failed"
        m <- run $ marketing hname smail
        return()

regist :: (l1 :~: Register, l2 :~: Register) => Labled Nil DB -> Labled l1 String -> Labled l2 String -> PIO Nil Bool
regist db name mail = do
                let m = unLabled mail
                let n = unLabled name
                res <- userExist db name
                if res then return False else return True

userExist :: (l1 :~: Register) => Labled Nil DB -> Labled l1 String -> PIO Nil Bool
userExist db name  = return True

marketing :: (l1 :~: Ads, l2 :~: Ads) => Labled l1 String -> Labled l2 String -> PIO Nil Bool
marketing name string = return True

hashName :: (l1 :~: Register) => Labled l1 String -> Labled Nil String
hashName name = do
                let n = unLabled name
                return (show $ hash n) :: Labled Nil String
                