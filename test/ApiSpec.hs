{-# LANGUAGE OverloadedStrings #-}
module ApiSpec (main, spec) where

import Test.Hspec
import Test.Hspec.Wai((<:>),with, get, shouldRespondWith, matchHeaders)


import Api(app)

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (return app) $ do
  describe "GET /?input=25" $ do
    it "responds with 400" $ do
      get "/?input=25" `shouldRespondWith` 400

  describe "GET ?input[]=2" $ do
    it "responds with 200" $ do
      get "/?input[]=2" `shouldRespondWith` "[\"a\",\"b\",\"c\"]"

  describe "GET ?input[]=2,5" $ do
    it "responds with 400" $ do
      get "/?input[]=2,5" `shouldRespondWith` 400

  describe "GET ?input[]=2&input[]=5" $ do
    it "responds with 400" $ do
      get "/?input[]=2&input[]=5" `shouldRespondWith`
        "[\"aj\",\"ak\",\"al\",\"bj\",\"bk\",\"bl\",\"cj\",\"ck\",\"cl\"]"

  describe "GET ?input[]=1&input[]=2&input[]=0" $ do
    it "responds with 400" $ do
      get "/?input[]=1&input[]=2&input[]=0" `shouldRespondWith`
        "[\"a\",\"b\",\"c\"]"

  describe "GET ?input[]=0" $ do
    it "responds with no words" $ do
      get "/?input[]=0" `shouldRespondWith`
        "[]"
