module HW8 where

e0 :: IO String
e0 = do return "Hello World"

e0' :: String -> String
e0' s = s

-- e1
putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = putChar x >> putStr' xs

-- e2
putStrLn0 :: String -> IO ()
putStrLn0 [] = putChar '\n'
putStrLn0 xs = putStr' xs >> putStrLn0 ""

putStrLn1 :: String -> IO ()
putStrLn1 [] = putChar '\n'
putStrLn1 xs = putStr' xs >> putChar '\n'

putStrLn2 :: String -> IO ()
putStrLn2 [] = putChar '\n'
putStrLn2 xs = putStr' xs >>= \x -> putChar '\n'

putStrLn5 :: String -> IO ()
putStrLn5 [] = putChar '\n'
putStrLn5 xs = putStr' xs >> putStr' "\n"

-- e3
getLine' = get []

get :: String -> IO String
get xs
  = do x <- getChar
       case x of
         '\n' -> return xs
         _ -> get (xs ++ [x])
