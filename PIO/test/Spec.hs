{-# LANGUAGE DataKinds #-}
module Spec where

import Labeled (Labeled, up)
import Purpose (Purpose( Register, Ads, All, Nil ))
import Test.Hspec

main :: IO()
main = runTestTTAndExit testSuits

username = return "testName" :: Labeled Register String
username' = return "testName" :: Labeled All String
usermail = return "TestMain" :: Labeled Ads String

testSuits = TestList [TestLabel "test1" test1]

test1 = TestCase (assertEqual "test" (up username) username')