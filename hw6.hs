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
