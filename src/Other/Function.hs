module Other.Function 
  ( sign
  , min'
  , modernise
  , letFun) 
  where

import Prelude hiding (Word)
import Unsafe.Coerce
import Data.Char (toLower, toTitle, toUpper)

-- Thinking in haskell
-- From high level to low level
-- Create a how to structure first like,
-- Focus on piece of piece of function
-- And you used haskell for what?

-- | Basic function
min' :: Int -> Bool
min' x 
  | x > 10 = True
  | otherwise = False

-- “To become a member of the Eq club we have to define an instance.”
-- (Num, Integral) w need instance from Num, Integral
sign :: (Num p, Integral a) => a -> p
sign x 
  |  h > t = 1
  |  h < t = -1
  |  h == t = 0
  | otherwise = undefined
 where (h, t) = (x `div` 100, x `mod` 100)

-- | Function Composition
{--
  Lets explain this (.)
  (.) :: (b -> c) -> (a -> b) -> a -> c
  f (x)  -> “function f to the argument x”

  (b -> c) function taking arg function (a -> b) alias arg b to c
  (a -> b) function taking arg a to b

  --> words(show(x))
  Rules: The order of composition is from right to left

  $ = taking argument of function
  . = taking result of function
--}
type Words = [Char]

play :: Show a => a -> [String]
play x = words . show $ x

words' :: Text -> [Words]
words' = error "not implemented"

-- | Exercise. Create to camel case function
-- “the morphology of prex” -> “The Morphology of Prex” 
type Word = String
type Text = String

toCamelCase :: Word -> Word
toCamelCase xs = [toTitle (head xs)] ++ tail xs

modernise :: Text -> Text
modernise = unwords . map toCamelCase . words

-- | Multiple let expression
letFun :: [Integer]
letFun = let a = [0..2] in
  let b = [3..10] in
  let s = a ++ b in
  filter (>2) s
