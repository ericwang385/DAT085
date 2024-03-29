{-# LANGUAGE DataKinds #-}

import Data.List
import Data.Maybe
import Test.Hspec
import Labeled (Labeled, up, join, dup, extract, down, (>>>=), hash)
import Purpose (Purpose( Register, Ads, All, Nil))

username = return "testName" :: Labeled Register String
username' = return "testName2" :: Labeled Register String
usermail = return "testMain" :: Labeled Ads String
usermail' = return "testMain2" :: Labeled Ads String

type DB = Labeled All [(Labeled Register String, Labeled Ads String)]
db = return [(username, usermail)] :: DB


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Basic function test" $ do
    it "Test up" $ do
      up username `shouldBe` (return "testName" :: Labeled Nil String)
    it "Test dup" $ do
      (dup . up) username `shouldBe` (return $ return "testName" :: Labeled Register (Labeled Ads String))
    it "Test join" $ do
      join (return $ return "testName" :: Labeled Register (Labeled Ads String)) `shouldBe` (return "testName" :: Labeled Nil String)
  describe "Registration example" $ do
    it "Test existUser" $ do
      existUser db username `shouldBe` (return True :: Labeled Register Bool)
    it "Test updateDB" $ do
      let res = existUser db username'
      if extract res then res `shouldBe` (return True :: Labeled Register Bool)
      else do
        let newDB = updateDB db username' usermail'
        newDB `shouldBe` (return [(username, usermail), (username', usermail')] :: DB)
    it "Test Regist" $ do
      register db username usermail `shouldBe` (return True :: Labeled Nil Bool)

existUser :: DB -> Labeled Register String -> Labeled Register Bool
existUser db name = db >>>= \db' -> mapM fst db' >>>= --All
                            \nameList -> name >>>=
                            \name' -> return $ isJust $ find (==name') nameList

updateDB :: DB -> Labeled Register String -> Labeled Ads String -> DB
updateDB db name mail = do
              db' <- db
              return $ db' ++ [(name, mail)]

sendAds :: Labeled Ads String -> Labeled Ads String -> Labeled Ads Bool
sendAds hname mail = do
            mail' <- mail
            if length mail' > 2 then return True
            else return False

sendConfirm :: Labeled Register String -> Labeled Register String -> Labeled Register Bool
sendConfirm name mail = do
            mail' <- mail
            if length mail' > 2 then return True
            else return False

register :: DB -> Labeled Register String -> Labeled Ads String -> Labeled Nil Bool
register db name mail = existUser db name >>>= \res ->
                        if not res then updateDB db name mail >>>= \_ -> return True
                        else undefined >>>= \_ ->
                        let hname = hash name in sendAds (down hname) mail >>>= \res -> return res




