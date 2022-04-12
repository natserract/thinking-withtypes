module Main where
import Data.Char (toLower, toTitle, toUpper)
import Prelude hiding (Word)
import Unsafe.Coerce

-- import Lib

-- Thinking in haskell
-- From high level to low level
-- Create a how to structure first like,
-- Focus on piece of piece of function
-- And you used haskell for what?
-- Customers
data Customers
data Products

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
 -- definisi lokal yg konteksnya/scopenya keseluruhan dari sisi kana
 where (h, t) = (x `div` 100, x `mod` 100)

-- Type Classes
data Person = Individual | Group
instance Eq Person where
  Individual == Individual = True
  Group == Group = True
  _ == _ = False

checkPerson :: Person -> Bool
checkPerson n 
  | n == Individual = True
  | otherwise = False

-- Define Subclass
-- class (Eq a, Show a) => Num a where
--  (+),(-),(*) :: a -> a -> a”

{--
  # Function Composition
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
play :: Show a => a -> [String]
play x = words . show $ x

-- Real case, sum array
type Word' = [Char]

words' :: Text -> [Word']
words' = error "not implemented"

-- Input: “The morphology of prex”
-- Output: tocamelcase
-- toCamelCase :: String -> String
type Word = String
type Text = String

toCamelCase :: Word -> Word
toCamelCase xs = [toTitle (head xs)] ++ tail xs

modernise :: Text -> Text
modernise = unwords . map toCamelCase . words
-- map toTitle . map head . words
-- tail_case = unwords . map tail . words

-- "Hello there, Mom!"
-- Count Capt

data Graph a b = Empty | Context a b

fun = let a = [0..2] in
  let b = [3..10] in
  let s = a ++ b in
  filter (>2) s

data Point = Point Double Double
  deriving (Eq, Show)

-- Product and Sum Types
{-
# Sum Types:
  tipe penjumlahan adalah cara untuk menyatukan tipe dasar untuk membuat tipe yang lebih kompleks

  Dengan kata lain, menggunakan tipe penjumlahan seperti mengatakan bahwa Anda membutuhkan tipe a atau tipe b: “Saya membutuhkan Benar atau Salah”, “Saya membutuhkan titik 2D atau titik 3D”, dll.

  Product types:
  data (,) a b = (,) a b

  Sum Tyoes
  data Bool = False | True 
  False = 1
  True = 1
  Means: False | True = 1 + 1

  data Light = Green | Yellow | Red 
  Product is `Light`
  ADTs = Data constructor 3,
        2*3 = 8
      True 
-}
showPoint :: Point
showPoint = Point 3 4

-- Exercise
data Person' = Person'{
  fullName:: String
  , address:: String
  , phoneNumber:: String
} 
instance Show Person' where
  -- unsafeCoerce = Coerce a value from one type to another, 
  -- bypassing the type-checker.
  show p = "Person" <> (unsafeCoerce p)

getPersonName :: String -> String
getPersonName s = show $ Person' { 
  fullName = s,
  address = "",
  phoneNumber = ""
}

main :: IO ()
main = do
  print $ map toLower "Words"
  print $ sign 10
  print $ checkPerson Individual
  print $ toTitle 'a'
  print $ modernise "the morphology of prex"
  print $ fun
  print $ getPersonName "Benjamin"
  
