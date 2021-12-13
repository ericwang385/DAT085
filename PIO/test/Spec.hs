{-# LANGUAGE DataKinds #-}

import Test.Hspec
import Data.List
import Data.Char
import Data.Maybe
import Data.Hashable
import Labeled (Labeled, up, join, dup, com, extract, unLabeled, down)
import Purpose (Purpose( Register, Ads, All, Nil ))

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
      existUser db username `shouldBe` (return True :: Labeled All Bool)
    it "Test updateDB" $ do
      let res = existUser db username'
      if extract res then res `shouldBe` (return True :: Labeled All Bool)
      else do
        let newDB = updateDB db username' usermail'
        newDB `shouldBe` (return [(username, usermail), (username', usermail')] :: DB)
    it "Test Regist" $ do
      register db username usermail `shouldBe` (return True :: Labeled All Bool)


existUser :: DB -> Labeled Register String -> Labeled All Bool
existUser db name = do
              let db' = unLabeled db
              let nameList = map fst db'
              return $ isJust $ find (==name) nameList

updateDB :: DB -> Labeled Register String -> Labeled Ads String -> DB
updateDB db name mail = do
              let db' = unLabeled db
              return $ db' ++ [(name, mail)]

sendMail :: Labeled Ads String -> Labeled Ads String -> Labeled All Bool
sendMail hname mail = do
            let mail' = extract mail
            if length mail' > 8 then return True
            else return False

hashName :: Labeled Register String -> Labeled Nil String
hashName = return . show . hash . extract

register :: DB -> Labeled Register String -> Labeled Ads String -> Labeled All Bool
register db name mail = do
            let res = existUser db name
            if (not . unLabeled) res
              then updateDB db name mail
            else undefined
            let hname = hashName name
            sendMail (down hname) mail
            return True
