module DialpadSpec (main, spec) where

import Test.Hspec
import Dialpad
import Test.QuickCheck(Arbitrary, arbitrary, elements, property, shrink, subterms)

instance Arbitrary PhoneDigit where
  arbitrary = elements [Zero, One, Two, Three, Four, Five, Six, Seven, Eight, Nine]
  shrink Zero = []
  shrink One = [Zero]
  shrink Seven = [Zero, One]
  shrink Nine = [Zero, One, Seven]
  shrink n = subterms n
  -- TODO shrink?

spec :: Spec
spec = do
  describe "telephoneWords" $ do
    it "disregards 0 and 1 in the digit sequence" $ do
      property $ \n -> telephoneWords n == telephoneWords (filter (> One) n)
--      telephoneWords [One, Two] == telephoneWords [Two]

main :: IO ()
main = hspec spec
