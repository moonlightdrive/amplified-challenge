module Dialpad
--  (digitsFromString, telephoneWords)
where

import Control.Monad
import Data.Char(isNumber, digitToInt)


data PhoneDigit = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine
  deriving (Show, Eq, Ord)

-- too rethink types?
telephoneMapping :: PhoneDigit -> [Char]
telephoneMapping Zero  = ""
telephoneMapping One   = ""
telephoneMapping Two   = "abc"
telephoneMapping Three = "def"
telephoneMapping Four  = "ghi"
telephoneMapping Five  = "jkl"
telephoneMapping Six   = "mno"
telephoneMapping Seven = "pqrs"
telephoneMapping Eight = "tuv"
telephoneMapping Nine  = "wxyz"

hasMapping :: PhoneDigit -> Bool
hasMapping digit =
  case digit of
    Zero      -> False
    One       -> False
    otherwise -> True

-- TODO rename this also make it more legible
digitsFromString :: String -> Maybe [PhoneDigit]
digitsFromString =
  let buttons = [Zero, One, Two, Three, Four, Five, Six, Seven, Eight, Nine]
  in mapM (\c -> do { guard (isNumber c); return (buttons !!  digitToInt c) })


telephoneWords :: [PhoneDigit] -> [String]
telephoneWords = sequence . fmap telephoneMapping
