-- |
-- Module      : Dialpad
-- Description : A library for handling telephone digit inputs.
--
-- A library for handling telephone digit inputs.
--
-- = A Valid Sequence of Phone Digits
-- An array of integers represents a valid sequence of phone digits
-- if it only contains one-digit non-negative integers
--
-- The following are all invalid sequences:
--
-- * [2,15]
-- * [-1, 2, 5]
module Dialpad (PhoneDigit(..),
                telephoneWords,
                telephoneMapping) where

import Control.Monad
import Data.Char(isNumber, digitToInt)


-- | A type representing the digit buttons of a telephone keypad
data PhoneDigit = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine
  deriving (Show, Eq)

-- | Maps a digit to a list of the characters it represents.
--
-- This mapping is consistent with that of a modern phone,
-- which maps letters Q and Z to numbers 7 and 9 respectively.
--
-- The digits 'Zero' and 'One' map to no characters. 
-- Therefore 'Zero' and 'One' behave like no-op, or buttons
-- which do nothing.
--
-- It follows that @telephoneMapping X = telephoneMapping Y@
-- where Y is a sequence of 'PhoneDigit's constructed by
-- removing all occurrences of 'Zero' and 'One' from X.
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
digitsFromIntList :: [Int] -> [PhoneDigit]
digitsFromIntList =
  let buttons = [Zero, One, Two, Three, Four, Five, Six, Seven, Eight, Nine]
  in map (\i -> buttons !!  i)

-- | Given a list of integers, return @True@ if it represents
-- a valid sequence of phone digits, or @False@ otherwise
isValidSequence :: [Int] -> Bool
isValidSequence ns = all (\n -> (n < 10) && (n >= 0)) ns

-- | Given a list of ints that represents a valid sequence of phone digits,
-- return a list of all possible words (including the gibberish ones!)
-- that the dialpad sequence can map to
--
-- >>> telephoneWords [2,5]
-- Just ["aj","ak","al","bj","bk","bl","cj","ck","cl"]
--
-- >>> telephoneWords [2,5,25]
-- Nothing
--
-- >>> telephoneWords [0]
-- Just []
telephoneWords :: [Int] -> Maybe [String]
telephoneWords ns =
  let validSeq = if isValidSequence ns
                 then return (filter hasMapping . digitsFromIntList $ ns)
                 else Nothing
  in case return . fmap telephoneMapping =<< validSeq of
       Just [] -> Just []                      -- do not want to return single word ""
       Nothing -> Nothing
       Just combos -> return (sequence combos)
