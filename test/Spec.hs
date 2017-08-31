{-# LANGUAGE OverloadedStrings #-}
import Api (app)
import Test.Hspec
import Test.Hspec.Wai(with, get, shouldRespondWith)


main :: IO ()
main = hspec $ with (return app) $ do
  describe "GET /phone" $ do
    it "responds with 400" $ do
      get "/phone" `shouldRespondWith` 400
