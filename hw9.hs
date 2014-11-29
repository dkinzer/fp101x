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


-- e3
mult m Zero = Zero
mult m (Succ n) = add0 m (mult m n)
e3 = natToInteger0 (mult one three) == natToInteger0 one * natToInteger0 three


-- e4
data Tree = Leaf Integer | Node Tree Integer Tree deriving Show

binary_tree_example = Node (Node (Leaf 1) 3 (Node (Leaf 4) 6 (Leaf 7))) 8 (Node (Leaf 9) 10 (Node (Leaf 13) 14 (Leaf 15)))

occurs0 :: Integer -> Tree -> Bool
occurs0 m (Leaf n) = m == n
occurs0 m (Node l n r)
  = case compare m n of
      LT -> occurs0 m l
      EQ -> True
      GT -> occurs0 m r

occurs1 :: Integer -> Tree -> Bool
occurs1 m (Leaf n) = m == n
occurs1 m (Node l n r)
  = case compare m n of
      LT -> occurs1 m r
      EQ -> True
      GT -> occurs1 m l

{- Fails with type mismatch
occurs2 :: Integer -> Tree -> Bool
occurs2 m (Leaf n) = compare m n
occurs2 m (Node l n r)
  = case compare m n of
      LT -> occurs2 m l
      EQ -> True
      GT -> occurs2 m r
-}

occurs3 :: Integer -> Tree -> Bool
occurs3 m (Leaf n) = m == n
occurs3 m (Node l n r)
  = case compare m n of
      LT -> occurs3 m l
      EQ -> False
      GT -> occurs3 m r

occurs4 :: Integer -> Tree -> Bool
occurs4 m (Leaf n) = m == n
occurs4 m (Node l n r)
  | m == n = True
  | m < n = occurs4 m l
  | otherwise = occurs4 m r

occurs5 :: Integer -> Tree -> Bool
occurs5 m (Leaf n) = m == n
occurs5 m (Node l n r)
  | m == n = True
  | m > n = occurs5 m l
  | otherwise = occurs5 m r

{- Type mismatch
occurs6 :: Integer -> Tree -> Bool
occurs6 m n = m == n
occurs6 m (Node l n r)
  | m == n = True
  | m < n = occurs6 m l
  | otherwise = occurs6 m r
-}

{- Type mismatch
occurs7 :: Integer -> Tree -> Bool
occurs7 m n = m == n
occurs7 m (Node l n r)
  | m == n = False
  | m < n = occurs7 m r
  | otherwise = occurs7 m l
-}


-- e5
data T = L Integer | N T T deriving Show

balanaced :: T -> Bool
leaves (L _) = 1
leaves (N l r) = leaves l + leaves r
balanaced (L _) = True
balanaced (N l r)
  = abs (leaves l - leaves r) <= 1 && balanaced l && balanaced r

e5_balanced = N (N (L 1) (L 3)) (N (L 2) (L 4))
e5_unbalanced = N (N (L 1) (L 3)) (N (L 2) (N (N (L 4) (L 6)) (L 5)))

-- e6
balance :: [Integer] -> T
halve xs = splitAt (length xs `div` 2) xs
balance [x] = L x
balance xs = N (balance ys) (balance zs)
  where (ys, zs) = halve xs

-- e7
data Expr = Add Expr Expr | Val Int deriving Show
