{-# LANGUAGE RebindableSyntax, GADTs, TypeFamilies, UndecidableInstances, MultiParamTypeClasses, ScopedTypeVariables, DataKinds #-}

import Data.List
import Data.Maybe
import Effect
import Monad
import Labeled (Labeled, up, join, dup, extract, down, hash, tag)
import Purpose (Purpose( Register, Ads, All, Nil))
import Prelude hiding (Monad(..))


username = tag "testName" Register
username' = tag "testName2" Register
usermail = tag "testMain" Ads
usermail' = tag "testMain2" Ads

type DB = Labeled All [(Labeled Register String, Labeled Ads String)]
db = tag [(username, usermail)] All


main :: IO ()
main = undefined

existUser :: DB -> Labeled Register String -> Labeled Register Bool
existUser db name = do
          db' <- db
          nameList <- mapM fst db'
          name' <- name
          return $ isJust $ find (==name') nameList
--existUser db name = db >>= \db' -> mapM fst db' >>= --All
--                                                \nameList -> name >>=
--                                                \name' -> return $ isJust $ find (==name') nameList




