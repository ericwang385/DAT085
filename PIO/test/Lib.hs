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
existUser :: Ref All DB -> Labeled Register String -> PIO Register Bool
existUser ref username = undefined

updateDB :: Ref All DB -> Labeled Register String -> Labeled Ads String -> PIO All ()
updateDB ref username usermail = undefined

readUsername :: IO() -> Labeled Register String
readUsername = undefined
