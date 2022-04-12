
-- | Chapter 1 - The Algebra Behind Types
-- https://wiki.haskell.org/Algebraic_data_type

-- `fst` hanya mengevaluasi nilai pertama
-- haskell is laziness
fst' :: String
fst' = fst ("no problems", True)

-- 1.1 Isomorphisms and Cardinalities
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

-- 1.2 Sum, Product and Exponential Types
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
-- You can called, x = horizontal, y = vertical, ex: (x1, x2, x3, y1, y2, y3, ..)
data TicTacToe a = TicTacToe
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
  -- ^ you can create like that, but haskell give us another beautiful solution => remove {}

-- (3. TTO) Analisis cardinality
-- TicTacToe => |TicTacToe a| = |a| x |a| x |a| .. (9 kali)
                          -- = |a|(9)
                          -- = |a|(3*3)