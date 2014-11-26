module HW6 where

-- e0
e0 = ([(2 *) x | x <- [1.. 10], even x]) ==  map (2 *) (filter even [1.. 10])

safetail :: [a] -> [a]
safetail [] = []
safetail (_:xs) = xs

-- e1
-- all :: (a -> Bool) -> [a] -> Bool
all0 :: (a -> Bool) -> [a] -> Bool
all0 p xs = and (map p xs)

all2 :: (a -> Bool) -> [a] -> Bool
all2 p = and . map p

all3 :: (a -> Bool) -> [a] -> Bool
all3 p = not . any (not . p)

all5 :: (a -> Bool) -> [a] -> Bool
all5 p xs = foldl (&&) True (map p xs)

all7 :: (a -> Bool) -> [a] -> Bool
all7 p = foldr (&&) True . map p

-- e2
any1 :: (a -> Bool) -> [a] -> Bool
any1 p = or . map p

any2 :: (a -> Bool) -> [a] -> Bool
any2 p xs = length (filter p xs) >  0

any3 :: (a -> Bool) -> [a] -> Bool
any3 p = not . null . dropWhile (not . p)

any5 :: (a -> Bool) -> [a] -> Bool
any5 p xs =  not (all (\x -> not (p x)) xs)

-- e3
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x : xs)
  | p x = x : takeWhile' p xs
  | otherwise = []

-- e4
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x : xs)
  | p x = dropWhile' p xs
  | otherwise = x : xs

-- e5
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

-- e6
filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if p x then x : xs else xs) []

-- e7
dec2int :: [Integer] -> Integer
dec2int = foldl (\x y -> 10 * x + y) 0

-- e8
compose = foldr (.) id

-- e9
