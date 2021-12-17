{-# LANGUAGE DataKinds #-}
module Lib where

import Data.List
import Data.Maybe
import Data.Hashable
import Labeled
import Purpose (Purpose( Register, Ads, All, Nil ))


type DB = Labeled All [(Labeled Register String, Labeled Ads String)]


existUser :: DB -> Labeled Register String -> Labeled Register Bool
existUser db name = db >>>= \db' -> mapM fst db' >>>= --All
                            \nameList -> name >>>=
                            \name' -> return $ isJust $ find (==name') nameList
--  do
--              db' <- db
--              name' <- name
--              nameList <- sequence . map fst db'
--              return isJust $ find (==name) nameList

updateDB :: DB -> Labeled Register String -> Labeled Ads String -> DB
updateDB db name mail = do
              db' <- db
              return $ db' ++ [(name, mail)]

sendMail :: Labeled Ads String -> Labeled Ads String -> Labeled Ads Bool
sendMail hname mail = do
            let mail' = extract mail
            if length mail' > 2 then return True
            else return False

hashName :: Labeled Register String -> Labeled Nil String
hashName = return . show . hash . extract

register :: DB -> Labeled Register String -> Labeled Ads String -> Labeled Nil Bool
register db name mail = existUser db name >>>= \res ->
                        if not res then updateDB db name mail >>>= \_ -> return True
                        else undefined >>>= \_ ->
                        let hname = hashName name in sendMail (down hname) mail >>>= \res -> return res
--  do
--            res <- up $ existUser db name
--            if not res
--              then updateDB db name mail >>>= \_ -> return True
--            else undefined
--            let hname = hashName name
--            up $ sendMail (down hname) mail