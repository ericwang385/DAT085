{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

import Ref
import Labled
import PIO
import Purpose
import Data.IORef

type DB = [(Labled Register String, Labled Register String)]

main :: IO ()
main = do
    putStrLn "user Regrister program"
    ref <- newIORef []
    let db = toRef ref :: Ref Nil DB
    putStrLn "Enter User Name:"
    name <- getLine
    let pname = return name :: Labled Register String
    putStrLn "Enter User mail:"
    mail <- getLine
    let pmail = return mail :: Labled Ads String
    regist ref name mail
    return ()

regist :: ((Flows l1 Register), (Flows l2 Register)) => Ref l DB -> Labled l1 String -> Labled l2 String -> Labled l Bool
regist = undefined
