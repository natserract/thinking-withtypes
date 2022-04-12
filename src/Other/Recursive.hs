module Recursive
    ( someFunc
    ) where

import Data.List;
import Data.Char;

import Debug.Trace (trace)

-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- filter:: (a -> Bool) -> [a] -> [a] 
-- not :: Bool -> Bool

someFunc :: IO ()
someFunc = do
    text <- readFile "./hamlet.txt";
    let ws = words $ map toLower text
    let ws' = map (takeWhile isLetter . dropWhile (not . isLetter)) ws
    let ws'' = map head $ group $ sort $ words $ map toLower text
    -- let cleanedWords = filter (not . null) ws'
    -- null [1, 2, 3]
    -- (\x -> not (null x))
    -- not . null = null x
    -- not = a, null = b
    -- null x ->
    let cleanedWords = filter (\x -> not (null x)) ws'
    print cleanedWords




{--
   Think recursive in haskell.
   Recursive: function called itself

  Recursive (like loops need to stop)
       - It's a base case 
--}

-- Basic 1, learn about lists
-- head: remove last element, tail: remove first element
teams = ["red", "yellow", "orange"]

{--
  The process:
  Stop: when a = []

  Think in loops/map
  1 + length' ["yellow", "orange"] =
    1 + length "yellow" -> index 0 => 1 + 1 + ["orange"]
    1 + length "orange" -> index 1 => 1 + 1 + 1 + []
    Calculate: 1 + 1 + 1 + 0 = 3
-}
length' :: [a] -> Int
length' [] = 0
length' (x:xs) = do
     1 + length' xs


{--
  The process:
  Stop: when n = 0

  5 * factorial 4
  5 * (4 * (factorial 3))
  5 * (4 * (3 * (factorial 2))
  5 * (4 * (3 * (2 * (factorial 1))))
  5 * (4 * (3 * ( 2 * (1 * (factorial 0))))) -- base case
  5 * (4 * (3 * ( 2 * ( 1 * 1))))
  ...
  Result: 120
--}
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Highlevel way
take' :: Int -> [a] -> [a]
take' 0 _ = []
take' n [] = []
take' 1 (x: _) = [x]
take' n (x:xs) = x : take' (n-1) xs

{--
 Parser:
 - AST (Abstract Syntax Tree)
 - Lexical analysis: transform string to list of tokens
 - Tokens: List structure such as numbers, string constants, identifiers, separators
 - Parsi(ng/er): list of tokens transform to abstract syntax tree
--}
