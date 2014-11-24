module HW5 where


-- e0
m !^ 0 = 0
m !^ n = m * m !^ (n - 1)

m @^ 0 = 1
m @^ n = m * m @^ (n - 1)

m #^ 0 = 1
m #^ n = m * m #^ n - 1

m $^ 0 = 1
m $^ n = n * n $^ (m - 1)

m %^ 0 = 1
m %^ n = m * (^) m (n - 1) 

-- e4
and0 [] = True
and0 (b : bs) = b && and bs

and1 [] = True
and1 (b : bs) 
  | b = and1 bs
  | otherwise = False

and4 [] = True
and4 (b : bs) 
  | b == False = False
  | otherwise = and4 bs

and6 [] = True
and6 (b : bs) = and6 bs && b

-- e6
replicate' 0 _ = []
replicate' n x = x : replicate' (n - 1) x

-- e7
(x : _) @!! 0 =  x
(_ : xs) @!! n = xs @!! (n - 1)

-- e8
elem' _ [] = False
elem' x (y : ys)
  | x == y = True
  | otherwise = elem' x ys

-- e9
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys)
  = if x <= y then x : merge xs (y : ys) else y : merge (x : xs) ys

-- e10 
halve :: [a] -> ([a], [a])
halve xs = splitAt(length xs `div` 2) xs

msort [] = []
msort [x] = [x]
msort xs = merge (msort ys) (msort zs)
  where (ys, zs) = halve xs
