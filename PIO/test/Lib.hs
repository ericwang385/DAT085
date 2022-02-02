{-# LANGUAGE DataKinds #-}
module Lib where

import Data.List
import Data.Maybe
import Data.Hashable
import PIO
import Labeled
import Ref
import Purpose (Purpose( Register, Ads, All, Nil ))

type DB = [(Labeled Register String, Labeled Ads String)]
--existUser :: Ref All DB -> Labeled Register String -> PIO Register Bool
--existUser ref username = do
--                      db <- readRefPIO ref
--                      name <- toPIO username
--                      let db' = unLabeled db
--                      let nameList = mapM fst db'
--                      names <- toPIO nameList
--                      return $ isJust $ find(==name) names
--
--
--updateDB :: Ref All DB -> Labeled Register String -> Labeled Ads String -> PIO All ()
--updateDB ref username usermail = do
--                            db <- readRefPIO ref
--                            let db' = unLabeled db
--                            writeRefPIO ref ((username, usermail): db')
