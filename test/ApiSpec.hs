{-# LANGUAGE OverloadedStrings #-}
module ApiSpec (main, spec) where

import Test.Hspec
import Test.Hspec.Wai(with, get, shouldRespondWith)


import Api(app)

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (return app) $ do
  describe "GET /" $ do
    it "responds with 400" $ do
      get "/" `shouldRespondWith` 400

  describe "GET /?input=25" $ do
    it "responds with 200" $ do
      get "/?input=25" `shouldRespondWith` 200
