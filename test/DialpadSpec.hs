module DialpadSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck (property)

import Dialpad


spec :: Spec
spec = do
  describe "telephoneWords" $ do
    it "disregards 0 and 1 in the digit sequence" $ do
      (property $ \n -> telephoneWords n == telephoneWords (filter (`notElem` [0, 1]) n));

main :: IO ()
main = hspec spec
