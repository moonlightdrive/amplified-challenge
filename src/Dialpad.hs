module Dialpad
--  (digitsFromString, telephoneWords)
where

import Control.Monad
import Data.Char(isNumber, digitToInt)


data PhoneDigit = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine
  deriving (Show, Eq, Ord, Enum)

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

-- TODO do you really agree this is readable
telephoneWords :: String -> Maybe [String]
telephoneWords = digitsFromString
                 >=> return . (sequence . fmap telephoneMapping . filterNoOps)
  where filterNoOps = filter (> One)
