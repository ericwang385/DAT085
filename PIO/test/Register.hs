{-# LANGUAGE DataKinds #-}

module Register where

import Lib
import Ref
import PIO
import Labeled
import Data.IORef
import Purpose(Purpose(Register, Ads, All, Nil), Join)

testname = return "testName" :: Labeled Register String
testname' = return "testName2" :: Labeled Register String
testmail = return "testMail" :: Labeled Ads String
testmail' = return "testMail2" :: Labeled Ads String

--
--regist :: IO ()
--regist = do
--    putStrLn "Example program for PIO"
--    testdb <- newIORef [(testname, testmail)]
--    putStrLn "Enter username"
--    username <- getLine
--    putStrLn "Enter usermail"
--    usermail <- getLine
--    let db = createRef testdb :: Ref All DB
--    let username' = return username :: Labeled Register String
--    let usermail' = return usermail :: Labeled All String
--    res_exist <- run $ existUser db username'
--    if extract res_exist then putStrLn "User already exist"
--    else do
--      run $ updateDB db username' (up usermail')
--      putStrLn "Register success"
--      return ()
