{-# LANGUAGE TypeFamilies #-}

module Modules.Chapter02 where
import Data.Functor ((<&>))
import Data.Kind (Constraint)
import Control.Monad

-- | Chapter 2 - TERMS,TYPES AND KINDS
-- 
-- In everyday Haskell programming, the fundamental 
-- building blocks are those of terms and types.
-- 
-- Terms: the values you can manipulate—the things that exist at runtime.
-- Types: little more than sanity-checks: proofs to the compiler (and ourselves)
-- 
-- The fundamental building blocks for `type-level programming` are types and kinds.
-- 
-- Kind: the type system for types / the types of types
-- Types: the things to manipulate

-- | Kinds (denoted * and called 'type')
-- (*) that the type is a concrete type. A concrete type is a type that doesn't take any type parameters and values can only have types that are concrete types.
-- 
-- You can check kind in prelude using :kind ...
-- 
-- # Nullary type constructors
-- 
-- Integer
-- Have kind `*` it's mean: kind of all nullary type constructors
kind :: Integer
kind = 5

-- # Unary type constructors
-- 
-- Maybe
-- Have kind `* -> *` : kind with type parameter
-- it's called unary type constructor, because have one concrete type parameters,
-- 
-- Maybe :: * -> *
-- Maybe Int :: *
kindT' :: Maybe Int -- type parameters solved
kindT' = Just 1

-- Either:: * -> * -> *
-- have two concrete type parameters 
kindT2' :: Either Int Bool
kindT2' = Left 1

-- # Arrow kinds
-- same like either, have two concrete type parameters 
-- (->): * -> * -> *

-- # Higher-kinded types (HKTs)
-- `(* -> *) -> *`
-- `* -> (* -> *)`
-- 
-- Rules: Everything above order/rank 1 is called higher-order, higher-rank or higher-kinded
-- 
-- Functor :: (* -> *) -> Constraint
-- Monad :: (* -> *) -> Constraint
hkt :: Maybe String
hkt = (Just 3) <&> show

-- # Constraint kinds
-- Constraints (which appear in types to the left of the => arrow) have a very restricted syntax
-- More generally, CONSTRAINT is the kind of any fully-saturated typeclass
-- 
-- Rules:
-- - Class constraints, e.g. Show 
-- - Implicit parameter constraints, e.g. ?x::Int (with the -XImplicitParams flag)
-- - Equality constraints, e.g. a ~ Int (with the -XTypeFamilies or -XGADTs flag)

contraintSw :: Show a => p -> a -> String
contraintSw a = show

-- type family
type family Typ a b :: Constraint
type instance Typ Int b = Show b
type instance Typ Bool b = Num b

contraintTf' :: Typ a b => a -> b -> b
contraintTf' = error "(contraintTf'): not implemented"

-- Exercise 2.1.3-i
-- Question: If Show Int has kind CONSTRAINT, what’s the kind of Show?
-- Answer: * -> Constraint

-- Exercise 2.1.3-ii
-- Question: What is the kind of Functor?
-- Answer: (* -> *) -> Constraint

-- Exercise 2.1.3-iii
-- Question: What is the kind of Monad?
-- Answer: (* -> *) -> Constraint

-- Exercise 2.1.3-iv
-- Question: What is the kind of MonadTrans?
-- Answer: (t :: (* -> *) -> * -> *) (m :: * -> *)


-- References:
-- https://kseo.github.io/posts/2017-01-13-constraint-kinds.html