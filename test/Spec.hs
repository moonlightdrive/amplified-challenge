{-# LANGUAGE OverloadedStrings #-}
import Control.Monad((>>))
import Test.Hspec
import Test.Hspec.Wai(with, get, shouldRespondWith)
import Test.QuickCheck(Arbitrary, arbitrary, elements, property)

import Api (app)
import Dialpad

instance Arbitrary PhoneDigit where
  arbitrary = elements [Zero, One, Two, Three, Four, Five, Six, Seven, Eight, Nine]
  -- TODO shrink?

dialpadTests :: Spec
dialpadTests = describe "telephoneWords" $ do
  it "disregards 0 and 1 in the digit sequence" $ do
    property $ \n -> telephoneWords n == telephoneWords (filter (> One) n)

restTests :: Spec
restTests = with (return app) $ do
  describe "GET /phone" $ do
    it "responds with 400" $ do
      get "/phone" `shouldRespondWith` 400

  describe "GET /phone?input=25" $ do
    it "responds with 200" $ do
      get "/phone?input=25" `shouldRespondWith` 200

main :: IO ()
main = hspec $ dialpadTests >> restTests
