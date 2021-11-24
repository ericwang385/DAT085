{-# LANGUAGE GADTs, DataKinds, TypeOperators #-}

import Labled ( Labled )
import PIO ( PIO, run )
import Purpose ( (:~:), BasicPurpose, Higher, Lower )

main :: IO ()
main = test

type Name = String
type Register = BasicPurpose
type Ads = Higher Register

test :: IO()
test = do   
        putStr "Input user name:"
        name <- getLine 
        -- user name can only be used in register
        let sname = return name :: Labled Register String
        -- hashed name can be used in marketing
        hashname <- run $ hashName sname
        putStr "Input user email"
        mail <- getLine
        let smail = return mail :: Labled Ads String
        r <- run $ regist sname smail
        m <- run $ marketing sname smail
        return()

regist :: (l1 :~: Register, l2 :~: Register) => Labled l1 String -> Labled l2 String -> PIO l Bool
regist = undefined

marketing :: (l1 :~: Ads, l2 :~: Ads) => Labled l1 String -> Labled l2 String -> PIO l Bool
marketing = undefined

hashName :: (l1 :~: Lower Ads) => Labled l1 String -> PIO l String
hashName = undefined