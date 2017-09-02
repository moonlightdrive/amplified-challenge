-- |
-- Module      : Dialpad
-- Description : A library for handling telephone digit inputs.
--
-- = A Valid Sequence of Phone Digits
-- This module currently considers a valid sequence of phone digits
-- to be a string that /only/ contains numeric characters,
-- or is the empty string ""
--
-- The following are all invalid sequences:
--
-- * "a25"
-- * "2,5"
module Dialpad (PhoneDigit(..),
                digitsFromString,
                telephoneWords) where

import Control.Monad
import Data.Char(isNumber, digitToInt)


-- | A type representing the digit buttons of a telephone keypad
data PhoneDigit = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine
  deriving (Show, Eq)

-- | Maps a digit to a list of the characters it represents.
--
-- This mapping is consistent with that of a modern phone,
-- which maps letters Q and Z to numbers 7 and 9 respectively.
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

-- | Determines whether a phone digit key corresponds to any letters.
hasMapping :: PhoneDigit -> Bool
hasMapping digit =
  case digit of
    Zero      -> False
    One       -> False
    otherwise -> True

-- TODO rename this also make it more legible
-- | Convert a string to 'PhoneDigit's
-- if that string represents a valid sequence of phone digits.
--
-- >>> digitsFromString "25"
-- Just [Two, Five]
--
-- >>> digitsFromString "ab34"
-- Nothing
--
-- >>> digitsFromString "2,5"
-- Nothing
digitsFromString :: String -> Maybe [PhoneDigit]
digitsFromString =
  let buttons = [Zero, One, Two, Three, Four, Five, Six, Seven, Eight, Nine]
  in mapM (\c -> do { guard (isNumber c); return (buttons !!  digitToInt c) })

-- TODO do you really agree this is readable
-- | Given an input string that represents a sequence of phone digits,
-- return a list of all possible words (including the gibberish ones!)
-- that the dialpad sequence can map to
--
-- >>> telephoneWords "25"
-- Just ["aj","ak","al","bj","bk","bl","cj","ck","cl"]
--
-- >>> telephoneWords "ab34"
-- Nothing
--
-- >>> telephoneWords "2,5"
-- Nothing
telephoneWords :: String -> Maybe [String]
telephoneWords = digitsFromString
                 >=> return . (sequence . fmap telephoneMapping . removeNoOps)
  where removeNoOps = filter hasMapping
