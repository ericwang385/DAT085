{-# LANGUAGE RebindableSyntax, GADTs, TypeFamilies, UndecidableInstances, MultiParamTypeClasses, ScopedTypeVariables, DataKinds #-}

import Data.List
import Data.Maybe
import Effect
import Monad
import Purpose
import Labeled
import Prelude hiding (Monad(..))
import qualified Prelude as P
import Data.Type.Set (Set)

data Login
data Ads
type Register = Set '[Login, Ads]
type DB = Labeled All [(Labeled Register String, Labeled Login String)]

main :: IO()

main = do
--  name <- getLine
--  mail <- getLine
--  ans <- getLine

  let username = tag "Testname" :: Labeled Register String
  let usermail = tag "Testmail" :: Labeled Login String
  let db = tag [(username,usermail)] :: DB
  P.return ()

--existUser :: DB -> Labeled Register String -> Labeled Register Bool
--existUser db name = do
--            db' <- db
--            name' <- name
--            tagRegister False
--
--login :: DB -> Labeled Login String -> Labeled Login String -> Labeled Login Bool
--login = undefined
--
--regist :: DB -> Labeled Register String -> Labeled Login String -> Labeled Register Bool
--regist db name mail = do
--            ans <- existUser db name
--            case ans of
--              False -> tagRegister False
--              True -> do
--                      updateDB db name mail
--                      --sendMail mail
--                      tagRegister True
--              otherwise -> tagRegister False
--
--updateDB :: DB -> Labeled Register String -> Labeled Login String -> Labeled Register Bool
--updateDB = undefined
--
--sendMail :: Labeled Ads String -> Labeled Ads Bool
--sendMail = undefined
--
--tagRegister :: a -> Labeled Register a
--tagRegister = MkLabeled
--
--tagLogin :: a -> Labeled Login a
--tagLogin = MkLabeled
--
--tagAds :: a -> Labeled Ads a
--tagAds = MkLabeled


