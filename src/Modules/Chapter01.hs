module Modules.Chapter01 
  ( ticTacToeWinner
  , Player(..) 
  )
  where

import Data.Maybe (isJust)

-- | Chapter 1 - The Algebra Behind Types
-- https://wiki.haskell.org/Algebraic_data_type

-- `fst` hanya mengevaluasi nilai pertama
-- haskell is laziness
fst' :: String
fst' = fst ("no problems", True)

-- | 1.1 Isomorphisms and Cardinalities
--
-- ## Teory (Math):
-- - Kardinalitas: banyak anggota suatu himpunan, contoh: A = {1, 22, 3} dpt ditulis |A| = 3
-- - Isomorphisms: Struktur/pemetaan yang mempertahankan himpunan dan hubungan antar elemen
--
data Void
-- ^ Void kardinalitasnya 0, |Void| = 0

data Unit = Unit
-- ^ Unit kardinalitasnya 1, |Unit| = 1

data Bool' = Falsy | Truthy
-- ^ Bool' kardinalitasnya 2, |Bool'| = 2
-- 
-- ^# Composing Types Algebraically
-- SUM(OR): False = 1, True = 1, so => 1 + 1 = 2

-- Isomorphisms (∼=)
-- => Any two types that have the same cardinality will always be isomorphic to one another.
-- Keywords: inverse
data Spin = Up | Down
-- ^ cardinality -> 2

-- Rumus kardinalitas
-- |a->b| = |b|×|b|×···×|b| = |b|(kuadrat|a|)
          -- |a| times (dikali banyaknya a)
--
-- to isomorphisms
boolToSpin:: Bool -> Spin
boolToSpin True = Up
boolToSpin False = Down

spinToBool:: Spin -> Bool
spinToBool Up = True
spinToBool Down = False
--
-- In general, for any two types with cardinality n, there are n! unique isomorphisms between them

-- | 1.2 Sum, Product and Exponential Types
--
-- Type has 3 structure, 
-- 1. Product types = AND (x*y), 
  -- idea: "One value from type x AND one value from type y" 
  -- "product" is combination
--
-- 2. Sum types = OR/Either (x+y), 
  -- idea: "One value from type x OR one value from type y "
  -- "sum" is alternation
--
-- 3. Exponential Type = ??? (y^x), idea: ???

-- Let's say we have two types, "First Names" and "Last Names"
data FirstName = John | Mark | Michelle
data LastName = Smith | Martinez | Lee

-- If we wanted to combine the two together into a new type called "Full Names", 
-- we would use "AND" for the Types and multiplication for the values
-- 
-- Constructor
data Pair = P Int Double

valPair :: Pair 
valPair = P 2 2.5
--

-- Product Types
data FullNamesAnd = FullNamesAnd FirstName LastName
                  -- ^ Tidak harus sama
-- Sum Types
data FullNamesOr = X FirstName | Y LastName

-- Define a function
startsWithJohn :: FullNamesAnd -> Bool
startsWithJohn a = case a of 
  FullNamesAnd John Smith -> True
  FullNamesAnd Mark Martinez -> False
  FullNamesAnd Michelle Lee -> False
  FullNamesAnd _ _ -> False
  -- ^ Possible values from `FirstName` and `LastName` is 9 (3x3)
  -- FullNamesAnd John ...
  -- FullNamesAnd ... Martinez 
  -- dan selanjutnya

-- Exercise 1.2-i:
-- Determine the cardinality of `Either Bool (Bool, Maybe Bool) -> Bool`
--- |Either Bool (Bool, Maybe Bool)| = |2| + (2, 1 + 2)

-- | 1.3 Example: Tic-Tac-Toe
-- Learn from build tictactoe game
-- Concept, 3 baris horizontal dan vertical
{-
  --------------
  |___|____|___|
  |___|____|___|
  |___|____|___|
-}
--
-- (1. TTO) First, start think of structure
-- You can called, x = horizontal, y = vertical, ex: (x1, x2, x3 ..)
data TicTacToe a = TicTacToe -- model of tictactoe schema position (high level)
  { topLeft:: a
  , topCenter:: a
  , topRight:: a
  , midLeft:: a
  , midCenter:: a
  , midRight:: a
  , bottomLeft:: a
  , bottomCenter:: a
  , bottomRight:: a
  }
-- ^ define per sisi
--
-- (2. TTO) Create empty function
emptyBoard :: TicTacToe (Maybe Bool)
emptyBoard = 
  TicTacToe
    Nothing Nothing Nothing
    Nothing Nothing Nothing
    Nothing Nothing Nothing
  -- TicTacToe {
  --   topLeft = Nothing
  --   ...
  -- }
  -- ^ you can create like that, but haskell give us another beautiful solution => without {}

-- (3. TTO) Analisis cardinality
-- TicTacToe => |TicTacToe a| = |a| x |a| x |a| .. (9 kali)
                          -- = |a|(9)
                          -- = |a|(3*3)
-- isomorphisms:
-- a -> (Three, Three)
-- (Three, Three) -> a

-- Re-analysis ...
-- Jika dilihat diatas, kita harus define posisinya satu2 (`data TicTacToe a` = ... )
-- Ini menjadi tidak efektif, bagaimana kalau struktur datanya banyak dan kompleks?
-- 
-- So, jika diperhatikan lagi TTO ini punya 3 baris vertical dan 3 bariz horizontal, 
-- kemudian disini hanya ada 2 pemain, dimana pemain bisa menempatkan tempat dimanapun
--
-- (Note: disini saya buat sedikit aga berbeda dari buku)
-- -- 1. Kita buat persiapan dlu, yaitu board nya
type Step = Int -- Just synonym (try to keep readable)
-- ^ Step adalah langkah pemain take position

data Board = Board (Step, Step, Step) deriving (Eq)
--
-- 2. Turun ke next down level, setelah persiapan board
-- Buat model play gamenya ...
--
-- Pemain (2 players) input step, (save to state)
data Player = Player1 | Player2 
instance Show Player where 
  show p = "(The win of tictactoe is: " <> show player <> " )"
    where 
      player = case p of
        Player1 -> "Player 1"
        Player2 -> "Player 2"

-- deriving (Eq, Show) -- automation instance

-- 2. check possibility
    {-
      [0, 1, 2],
      [3, 4, 5],
      [6, 7, 8],
      [0, 3, 6],
      [1, 4, 7],
      [2, 5, 8],
      [0, 4, 8],
      [2, 4, 6],
    -}
-- 3. Check winner
-- - The winner take stepnya harus berjumlah 3
-- - Jika contains dari list tersebut maka player win dan game stop

-- (4. TTO) modelling types input for function
-- You know, you must think everything is a function,
-- Perfunction punya tugas masing2 (SRP)
-- Before doing that, define input types first
isWin :: Board -> Maybe Bool
isWin t 
  | t == Board (0, 1, 2) || 
    t == Board (3, 4, 5) ||
    t == Board (6, 7, 8) ||
    t == Board (0, 3, 6) ||
    t == Board (1, 4, 7) ||
    t == Board (2, 5, 8) ||
    t == Board (0, 4, 8) ||
    t == Board (2, 4, 6) = Just True
  | otherwise = Nothing

playTicTacToe :: Player -> String
playTicTacToe p
   | isJust (p1) = show $ Player1
   | isJust (p2) = show $ Player2
   | otherwise = show $ "Nothing"
  where 
    p1 = isWin (Board (2, 4, 6))
    p2 = isWin (Board (3, 6, 7))

ticTacToeWinner :: String
ticTacToeWinner = playTicTacToe $ Player1

-- | 1.4 The Curry–Howard Isomorphism
{-
  Algebra     Logic       Types
  a+b         a∨b         Either a b
  a×b         a∧b         (a, b)
  ba          a ==> b      a->b
  a=b         a <== b     isomorphism
  0           ⊥           Void
  1           ⊤           ()

  Theorem
  a1 = a
  () -> a and a

  Give a proof of (ab)c = ab×c. Does it remind you of anything from Prelude?
  Answer: (.) :: (b -> c) -> (a -> b) -> a -> c
-}
