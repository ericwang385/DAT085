{-# LANGUAGE TypeOperators #-}

import Data.List
import Data.Char
import Data.Maybe
import Labled ( Labled )
import PIO ( PIO, run, nilLabled)
import Purpose ( (:~:), Conj, All, Nil )

main :: IO ()
main = test

data Register
data Ads

test :: IO ()
test = do   
        putStr "Input user name:"
        name <- getLine 
        -- user name can only be used in register
        let sname = return name :: PIO Register String
        -- hashed name can be used in marketing
        let hname = hashName sname
        putStr "Input user email"
        mail <- getLine
        let smail = return mail :: PIO Ads String
        r <- run $ regist sname smail
        m <- run $ marketing hname smail
        return()

regist :: (l1 :~: Register, l2 :~: Ads) => PIO l1 String -> PIO l2 String -> PIO Nil Bool
regist name mail = do
                let m = nilLabled mail
                let n = nilLabled name
                n' <- n
                m' <- m
                return $ (length n' > 8) && isJust(find isDigit m')

marketing :: (l1 :~: Ads, l2 :~: Ads) => PIO l1 String -> PIO l2 String -> PIO Nil Bool
marketing = undefined

hashName :: (l1 :~: Register) => PIO l1 String -> PIO Nil String
hashName = undefined