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
curry' :: ((a, b) -> c) -> a -> b -> c
curry' f = \x y -> f (x, y)

-- e10
uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f = \(x, y) -> f x y

-- e11
unfold' :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> [a]
unfold' p h t x
  | p x = []
  | otherwise = h x : unfold' p h t (t x)

chop8 :: [Integer] -> [[Integer]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

chop8' :: [Integer] -> [[Integer]]
chop8'  = unfold' null (take 8) (drop 8)

-- e12
map'' :: (a -> b) -> [a] -> [b]
map'' f = unfold' null (f . head) tail

-- e13
iterate' :: (a -> a) -> a -> [a]
iterate' f = unfold' (const False) id f

-- e14
e14 = ((* 2) . ((3 +) . (/ 4))) 5 == (((* 2) . (3 +)) . (/ 4)) 5

-- e17
e17 = map (/ 2) (reverse [1..10]) == reverse (map (/ 2) [1..10])

-- e18
e18 = reverse [1..10] == reverse [5..10] ++ reverse [1..4]

-- e19
e19 = length (take 10 [1.. ]) == 10

-- e20
-- sum :: Num a => [a] -> a
-- sum is not a higher order function.

-- e21
-- map :: (a -> b) -> [a] -> [b]
-- map is not overloaded.

-- e22
-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr is not overloaded.

-- e23
-- take :: Int -> [a] -> [a]
-- take is polymorphic.

-- e24
-- (\x -> x > 3) :: (Ord a, Num a) => a -> Bool
-- The function is overloaded.

-- e25
e25 = take 4 (iterate (+1) 1) == [1..4]

-- e26
e26 = takeWhile even [2, 4, 5, 6, 7, 8] == [2,4]

-- e27
e27 = zip [1, 2] ['a', 'b', 'c'] == [(1,'a'),(2,'b')]

-- e28
e28 = foldr (-) 0 [1, 2, 3, 4] == -2

-- e29
e29 = filter even (map (+1) [1..5]) == [2, 4, 6]

-- e30
e30 = [(* 2) x | x <- [1..10], even ((* 2 ) x)] == filter even (map (* 2) [1..10])
