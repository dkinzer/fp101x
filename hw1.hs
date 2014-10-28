doubleMe x = x + x

doubleUs x y = x*2 + y*2

doubleSmallNumber x = if x < 100 then doubleMe x else x


n = a `div` length xs
  where
    a = 10
    xs = [1, 2, 3, 4, 5]


last1 s = drop (length s - 1) s
last2 s = head (last1 s)
last3 s = tail (reverse s)
last4 s = reverse (head s)
last5 s = s !! (length s - 1)
last6 s = head (drop (length s ) s)
last7 s = head (reverse s)
last8 s = reverse s !! (length s - 1)


f [] = [] 
f (x:xs) = f lesser ++ [x] ++ f greater
  where 
    lesser = [a | a <- xs, a <=  x] 
    greater = [b | b <- xs, b > x]

qsort1 [] = [] 
qsort1 (x:xs) = qsort1 greater ++ [x] ++ qsort1 lesser
  where 
    lesser = [a | a <- xs, a <=  x] 
    greater = [b | b <- xs, b > x]

qsort2 [] = [] 
qsort2 (x:xs) = reverse( qsort2 lesser ++ [x] ++ qsort2 greater )
  where 
    lesser = [a | a <- xs, a <=  x] 
    greater = [b | b <- xs, b > x]

qsort3 [] = [] 
qsort3 (x:xs) = qsort3 greater ++ qsort3 lesser ++ [x]
  where 
    lesser = [a | a <- xs, a <=  x] 
    greater = [b | b <- xs, b > x]

qsort4 [] = [] 
qsort4 (x:xs) = reverse(qsort4 lesser) ++ [x] ++ reverse(qsort4 greater)
  where 
    lesser = [a | a <- xs, a <=  x] 
    greater = [b | b <- xs, b > x]

qsort5 [] = [] 
qsort5 (x:xs) = qsort5 greater ++ [x] ++ qsort5 lesser
  where 
    lesser = [a | a <- xs, a <  x || a == x] 
    greater = [b | b <- xs, b > x]

qsort6 [] = [] 
qsort6 (x:xs) = qsort6 greater ++ [x] ++ qsort6 lesser
  where 
    lesser = [a | a <- xs, a <  x] 
    greater = [b | b <- xs, b > x]

qsort7 [] = [] 
qsort7 (x:xs)
  = reverse (reverse(qsort7 lesser) ++ [x] ++ reverse(qsort7 greater))
  where 
    lesser = [a | a <- xs, a <=  x] 
    greater = [b | b <- xs, b > x]

qsort8 [] = [] 
qsort8 (x:xs) = x : qsort8 greater ++ qsort8 lesser
  where
    x = maximum xs
    lesser = [a | a <- xs, a <  x] 
    greater = [b | b <- xs, b >= x]

g [] = [] 
g (x:xs) = g lesser ++ [x] ++ g greater
  where 
    lesser = [a | a <- xs, a <  x] 
    greater = [b | b <- xs, b > x]
