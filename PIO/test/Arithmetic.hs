{-#LANGUAGE DataKinds#-}
module Arithmetic where

import Labled (Labled)


test :: IO()
test = do   
        putStr "Input user name:"
        name <- getLine 
        let sname = return name :: Labled register String
        putStr "Input user email"
        mail <- getLine
        let smail = return mail :: Labled (Conj register ads) String
        r <- run $ regist sname smail
        m <- run $ marketing sname smail
        return()

regist :: ((Purpose l), (canFlowTo l register)) => Labled l String -> Labled l String -> Bool 
regist = undefined

marketing :: ((Purpose l), (canFlowTo l ads)) => Labled l String -> Bool 
marketing = undefined