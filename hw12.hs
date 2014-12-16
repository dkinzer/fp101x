module HW12  where

e0_last :: [a] -> a
e0_last (_ : xs) = e0_last xs
e0_last[x] = x

e0_foldr :: (a -> b -> b) -> b -> [a] -> b
e0_foldr f v (x : xs) = f x (e0_foldr f v xs)
e0_foldr _ v [] = v

e0_init :: [a] -> [a]
e0_init (x : xs) = x : e0_init xs
e0_init [_] = []

e0_drop :: Int -> [a] -> [a]
e0_drop n [] = []
e0_drop n (_ : xs) = e0_drop (n - 1) xs
e0_drop 0 xs = xs

(+++) ::  [a] -> [a] -> [a]
(x : xs) +++ ys = x : (xs +++ ys)
[] +++ ys = ys

e0_foldl :: (a -> b -> a) -> a -> [b] -> a
e0_foldl f v (x : xs) = e0_foldl f (f v x) xs
e0_foldl _ v [] = v
