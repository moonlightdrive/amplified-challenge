{-# LANGUAGE OverloadedStrings #-}
module ApiSpec (main, spec) where

import Test.Hspec
import Test.Hspec.Wai(with, get, shouldRespondWith)


import Api(app)

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (return app) $ do
  describe "GET /phone" $ do
    it "responds with 400" $ do
      get "/phone" `shouldRespondWith` 400

  describe "GET /phone?input=25" $ do
    it "responds with 200" $ do
      get "/phone?input=25" `shouldRespondWith` 200
