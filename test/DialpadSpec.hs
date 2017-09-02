module DialpadSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck (property)

import Dialpad


spec :: Spec
spec = do
  describe "telephoneWords" $ do
    it "disregards 0 and 1 in the digit sequence" $ do
      property $ \n -> telephoneWords n == telephoneWords (filter (`notElem` [0, 1]) n)

    it "returns no words for empty input" $ do
      telephoneWords [] == Just []

    it "returns no words for input that don't map to any characters" $ do
      telephoneWords [0,1] == Just []

main :: IO ()
main = hspec spec
