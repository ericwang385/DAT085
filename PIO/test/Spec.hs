{-# LANGUAGE RebindableSyntax, GADTs, TypeFamilies, UndecidableInstances, MultiParamTypeClasses, ScopedTypeVariables, DataKinds #-}

import Data.List
import Data.Maybe
import Effect
import Monad
import Purpose
import Labeled
import Prelude hiding (Monad(..))
import qualified Prelude as P

data Login
data Ads
data Register = Combine Login Ads
type DB = Labeled All [(Labeled Register String, Labeled Login String)]

main :: IO()

main = do
--  name <- getLine
--  mail <- getLine
--  ans <- getLine

  let username = tagRegister "Testname"
  let usermail = tagLogin "Testmail"
  let db = return [(username,usermail)]
  let ans = regist db username usermail
  P.return ()

existUser :: DB -> Labeled Register String -> Labeled Register Bool
existUser db name = do
            db' <- db
            name' <- name
            tagRegister False

login :: DB -> Labeled Login String -> Labeled Login String -> Labeled Login Bool
login = undefined

regist :: DB -> Labeled Register String -> Labeled Login String -> Labeled Register Bool
regist db name mail = do
            ans <- existUser db name
            case ans of
              False -> tagRegister False
              True -> do
                      updateDB db name mail
                      tagRegister True
              otherwise -> tagRegister False



updateDB :: DB -> Labeled Register String -> Labeled Login String -> Labeled Register Bool
updateDB = undefined

tagRegister :: a -> Labeled Register a
tagRegister = MkLabeled

tagLogin :: a -> Labeled Login a
tagLogin = MkLabeled

tagAds :: a -> Labeled Ads a
tagAds = MkLabeled


