{-# LANGUAGE NPlusKPatterns #-}

module HW9 where

import Data.List
import Data.Char
import Unsafe.Coerce

data Nat = Zero | Succ Nat deriving Show

-- e0
zero = Zero
one = Succ zero
two = Succ one
three = Succ two
four = Succ three

natToInteger0 :: Nat -> Integer
natToInteger0 Zero = 0
natToInteger0 (Succ n) = natToInteger0 n + 1

natToInteger1 :: Nat -> Integer
natToInteger1 (Succ n) = natToInteger1 n + 1
natToInteger1 Zero = 0

natToInteger2 :: Nat -> Integer
natToInteger2 n = natToInteger2 n -- BOTTOMS

natToInteger3 :: Nat -> Integer
natToInteger3 (Succ n) = 1 + natToInteger3 n
natToInteger3 Zero = 0

natToInteger4 :: Nat -> Integer
natToInteger4 Zero = 1
natToInteger4 (Succ n) = (1 + natToInteger4 n) - 1

natToInteger5 :: Nat -> Integer
natToInteger5 = head . m
  where m Zero = [0]
        m (Succ n) = [sum [x | x <- (1 : m n)]]

natToInteger6 :: Nat -> Integer
natToInteger6 = \n -> genericLength [c | c <- show n, c == 'S']

natToInteger7 :: Nat -> Int
natToInteger7 = \n -> length [c | c <- show n, c == 'S']

-- e1
integerToNat0 :: Integer -> Nat
integerToNat0 0 = Zero
integerToNat0 (n + 1) = Succ (integerToNat0 n)

integerToNat1 :: Integer -> Nat
integerToNat1 0 = Succ Zero
integerToNat1 n = (Succ (integerToNat1 n)) -- BOTTOMS

-- integerToNat2 :: Integer -> Nat
-- Does not typecheck ... integerToNat2 n = product [(unsafeCoerce c) :: Integer | c <- show n]

integerToNat3 :: Integer -> Nat
integerToNat3 n = integerToNat3 n -- BOTTOMS

integerToNat4 :: Integer -> Nat
integerToNat4 (n + 1) = Succ (integerToNat4 n)
integerToNat4 0 = Zero

integerToNat5 :: Integer -> Nat
integerToNat5 (n + 1) = let m = integerToNat5 n in Succ m
integerToNat5 0 = Zero

{- BOTTOMS
integerToNat6 :: Integer -> Nat
integerToNat6 = head . m
  where {
        ; m 0 = [0]
        ; m (n + 1) = [sum [x | x <- (1 : m n)]]
        }
-}

{- BOTTOMS
integerToNat7 :: Integer -> Nat
integerToNat7 =  \n -> genericLength [c | c <- show n, isDigit c]
-}

-- e2
add0 :: Nat -> Nat -> Nat
add0 Zero n = n
add0 (Succ m) n = Succ (add0 n m)

add1 :: Nat -> Nat -> Nat
add1 (Succ m) n = Succ (add1 n m)
add1 Zero n = n

add2 :: Nat -> Nat -> Nat
add2 Zero n = Zero
add2 (Succ m) n = Succ (add2 m n)

add3 :: Nat -> Nat -> Nat
add3 (Succ m) n = Succ (add3 m n)
add3 Zero n = n

add4 :: Nat -> Nat -> Nat
add4 n Zero = n
add4 n (Succ m) = Succ (add4 n m)

add5 :: Nat -> Nat -> Nat
add5 n (Succ m) = Succ (add5 n m)
add5 n Zero = n

add6 :: Nat -> Nat -> Nat
add6 n Zero = n
add6 n (Succ m) = Succ (add6 m n)

add7 :: Nat -> Nat -> Nat
add7 n (Succ m) = Succ (add7 m n)
add7 n Zero = n

