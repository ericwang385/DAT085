{-# LANGUAGE DataKinds #-}

import Data.List
import Data.Maybe
import Test.Hspec
import Prelude hiding (Monad(..))
import Labeled (Labeled, up, join, dup, extract, down, hash, tag)
import Purpose (Purpose( Register, Ads, All, Nil))

username = tag "testName" Register
username' = tag "testName2" Register
usermail = tag "testMain" Ads
usermail' = tag "testMain2" Ads

type DB = Labeled All [(Labeled Register String, Labeled Ads String)]
db = tag [(username, usermail)] All


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Registration example" $ do
    it "Test existUser" $ do
      existUser db username `shouldBe` tag True Register
    it "Test updateDB" $ do
      let res = existUser db username'
      if extract res then res `shouldBe` tag True Register
      else do
        let newDB = updateDB db username' usermail'
        newDB `shouldBe` tag [(username, usermail), (username', usermail')] All
--    it "Test Regist" $ do
--      register db username usermail `shouldBe` (return True :: Labeled Nil Bool)
--
existUser :: DB -> Labeled Register String -> Labeled Register Bool
existUser db name = do
              db' <- db
              name' <- name
              nameList <- sequence . map fst db'
              return isJust $ find (==name) nameList

updateDB :: DB -> Labeled Register String -> Labeled Ads String -> DB
updateDB db name mail = do
              db' <- db
              db' ++ [(name, mail)]
--
--sendAds :: Labeled Ads String -> Labeled Ads String -> Labeled Ads Bool
--sendAds hname mail = do
--            mail' <- mail
--            if length mail' > 2 then return True
--            else return False

--register :: DB -> Labeled Register String -> Labeled Ads String -> Labeled Nil Bool
--register db name mail = do
--            res <- up $ existUser db name
--            if not res
--              then updateDB db name mail >>>= \_ -> return True
--            else undefined
--            let hname = hashName name
--            up $ sendMail (down hname) mail




