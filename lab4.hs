{-# LANGUAGE NPlusKPatterns #-}
module Lab4 where

------------------------------------------------------------------------------------------------------------------------------
-- RECURSIVE FUNCTIONS
------------------------------------------------------------------------------------------------------------------------------

import Data.Char

-- ===================================
-- Ex. 0
-- ===================================

triangle :: Integer -> Integer
triangle 0 = 0
triangle (n + 1) = (n + 1) + triangle  n

e0 = (sum [0..500]) == triangle 500

-- ===================================
-- Ex. 1
-- ===================================

count :: Eq a => a -> [a] -> Int
count _ [] = 0
count a (b : xs)
  | a == b = 1 + count a xs
  | otherwise = count a xs

xs = [1,2,35,2,3,4,8,2,9,0,5,2,8,4,9,1,9,7,3,9,2,0,5,2,7,6,92,8,3,6,1,9,2,4,8,7,1,2,8,0,4,5,2,3,6,2,3,9,8,4,7,1,4,0,1,8,4,1,2,4,56,7,2,98,3,5,28,4,0,12,4,6,8,1,9,4,8,62,3,71,0,3,8,10,2,4,7,12,9,0,3,47,1,0,23,4,8,1,20,5,7,29,3,5,68,23,5,6,3,4,98,1,0,2,3,8,1]
ys = map (\x -> ((x + 1) * 3) ^ 3 - 7) xs

e3 = 14 == count 722 ys

poem = [ "Three Types for the Lisp-kings under the parentheses,"
       , "Seven for the Web-lords in their halls of XML,"
       , "Nine for C Developers doomed to segfault,"
       , "One for the Dark Lord on his dark throne"
       , "In the Land of Haskell where the Monads lie."
       , "One Type to rule them all, One Type to find them,"
       , "One Type to bring them all and in the Lambda >>= them"
       , "In the Land of Haskell where the Monads lie."
       ]

e4 = 16 == count 101 (poem >>= \x -> map (ord . \x -> chr (ord x + 4)) x)
-- ===================================
-- Ex. 2
-- ===================================

euclid :: (Int,  Int) -> Int
euclid (x, y)
  | x <= 0 || y <= 0 = 1
  | x == y = x
  | x < y = euclid (x, y - x)
  | otherwise = euclid (x - y, y)

e5 = 12 == euclid (13404, 8832)
e6 = 1 == euclid (1, 0)

-- ===================================
-- Ex. 3
-- ===================================

funkyMap :: (a -> b) -> (a -> b) -> [a] -> [b]
funkyMap _ _ [] = []
funkyMap f _ [a] = [f a]
funkyMap f g (a : b : xs) = f a : g b : funkyMap f g xs

-- e7
funkyMap_test = funkyMap (+10) (+100) [1, 2, 3, 4, 5] == [(+10) 1, (+100) 2, (+10) 3, (+100) 4, (+10) 5]
e7_a = (sum $ funkyMap (+10) (+100) ys) == 112319712
e7_b = (sum $ funkyMap (\c -> if c == 'e' then 1 else 0) ord (poem >>= id)) == 16805

-- e8
-- \a -> a :: t -> t

-- e9
-- [undefined] :: [t]

-- e10
-- (True, (False)) ::

-- e11
-- f :: t1 -> t -> (t, t1)

-- e12
-- foldr id :: b -> [b -> b] -> b

-- e13
-- flip foldr const
--   :: (a -> (a1 -> b -> a1) -> a1 -> b -> a1) -> [a] -> a1 -> b -> a1

-- e14
dup a = (a, a)
-- :t dup . dup . dup
-- dup . dup . dup :: a -> (((a, a), (a, a)), ((a, a), (a, a)))

-- e15
h g f = (f . g) $ f
-- h :: ((b -> c) -> b) -> (b -> c) -> c

-- e16
fix = h fix

-- e18
f :: (Integer -> Integer) -> Integer -> Integer
f = \f n -> if (n == 0) then 1 else n * f (n - 1)

-- e19
k :: Integer -> Integer
k = fix $ f
e19 = 1405006117752879898543142606244511569936384000000000 == (k 42)
