module HW8 where

e0 :: IO String
e0 = return "Hello World"

e0' :: String -> String
e0' s = s

e0'' :: IO Char
e0'' = do x <- e0
          return (head x)

-- e1
putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = putChar x >> putStr' xs

-- e2
putStrLn' :: String -> IO ()
putStrLn' [] = putChar '\n'
putStrLn' xs = putStr' xs >> putStrLn' ""

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

-- e4
interact' :: (String -> String) -> IO ()
interact' f = do input <- getLine'
                 putStrLn' (f input)
-- e5
e5_seq = [putStrLn' "1", putStrLn' "2", putStrLn' "3"]

sequence_1 :: Monad m => [m a] -> m ()
sequence_1 [] = return ()
sequence_1 (m:ms) = (foldl (>>) m ms) >> return ()

sequence_3 :: Monad m => [m a] -> m ()
sequence_3 [] = return ()
sequence_3 (m:ms) = m >> sequence_3 ms

sequence_4 :: Monad m => [m a] -> m ()
sequence_4 [] = return ()
sequence_4 (m:ms) = m >>= \_ -> sequence_4 ms

sequence_6 :: Monad m => [m a] -> m ()
sequence_6 ms = foldr (>>) (return ()) ms
