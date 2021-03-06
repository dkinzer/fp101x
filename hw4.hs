import Data.Char

-- Exercise 3
factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1 .. n], isPerfect x]
  where isPerfect num = sum (init (factors num)) == num

-- Exercise 5
find :: (Eq a) => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions :: (Eq a) => a -> [a] -> [Int]
positions x xs = find x  (zip xs [0 .. n])
  where n = length xs - 1

-- Exercise 7
let2int :: Char -> Int
let2int c = ord c - ord 'a'


int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c
  | isLower c = int2let ((let2int c + n) `mod` 26)
  | isUpper c = toUpper (shift n (toLower c))
  | otherwise = c

unshift :: Int -> Char -> Char
unshift n c
  | isLower c = int2let ((let2int c - n) `mod` 26)
  | isUpper c = toUpper (shift n (toLower c))
  | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

decode :: Int -> String -> String
decode n xs = [unshift n x | x <- xs]

-- Exercise 13
divides :: Int -> Int -> Bool
divides x y = x `mod` y == 0

divisors :: Int -> [Int]
divisors x = [d | d <- [1 .. x], x `divides` d]


e13answer = "\
  \Wow,\
  \\
  \This one kind of stumped me at first, until I realized that I could\
  \easily encode an upper case character by first encoding the lowercase\
  \form and upper casing the result. Or:\
  \\
  \Upper -> lower -> encode -> Upper.\
  \\
  \For good measure I added a decoder that does the opposite of encode."
