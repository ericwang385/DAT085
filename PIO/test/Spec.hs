{-# LANGUAGE DataKinds #-}

import Test.Hspec

import Lib
import Labeled (Labeled, up, join, dup, extract)
import Purpose (Purpose( Register, Ads, All, Nil ))

username = return "testName" :: Labeled Register String
username' = return "testName2" :: Labeled Register String
usermail = return "testMain" :: Labeled Ads String
usermail' = return "testMain2" :: Labeled Ads String
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




