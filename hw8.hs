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

sequence_' :: Monad m => [m a] -> m ()
sequence_' [] = return ()
sequence_' (m:ms) = (foldl (>>) m ms) >> return ()

-- e6
e6_seq = [putStrLn' "1", putStrLn' "2", putStrLn' "3"]

sequence'_0 :: Monad m => [m a] -> m [a]
sequence'_0 [] = return []
sequence'_0 (m : ms)
  = m >>=
      \ a ->
        do as <- sequence'_0 ms
           return (a : as)

{-- Fails with:
 - Couldn't match expected type `[a]' with actual type `()
 - In the first argument of `return', namely `()'
 - In the second argument of `foldr', namely `(return ())'
 - In the expression: foldr func (return ()) ms
sequence'_1 :: Monad m => [m a] -> m [a]
sequence'_1 ms = foldr func (return ()) ms
  where
    func :: (Monad m) => m a -> m [a] -> m [a]
    func m acc
      = do x <- m
           xs <- acc
           return (x : xs)
--}

{-- Fails with:
 - Could not deduce (m1 ~ [])
 - from the context (Monad m)
sequence'_2 :: Monad m => [m a] -> m [a]
sequence'_2 ms = foldr func (return []) ms
  where
    func :: (Monad m) => m a -> m [a] -> m [a]
    func m acc = m : acc
 --}

{-- Fails with: parse error on input ‘<-’
sequence'_3 :: Monad m => [m a] -> m [a]
sequence'_3 [] = return []
sequence'_3 (m : ms) = return (a : as)
  where
    a <- m
    as <- sequence'_3 ms
--}
sequence'_4 :: Monad m => [m a] -> m [a]
sequence'_4 ms = foldr func (return []) ms
  where
    func :: (Monad m) => m a -> m [a] -> m [a]
    func m acc
      = do x <- m
           xs <- acc
           return (x : xs)

{-- Fails with: Could not deduce (a ~ [a])
sequence'_5 :: Monad m => [m a] -> m [a]
sequence'_5 [] = return []
sequence'_5 (m : ms)
  = m >>
      \ a ->
        do as <- sequence'_5 ms
           return (a : as)
--}

{-- Fails with: parse error on input <-
sequence'_6 :: Monad m => [m a] -> m [a]
sequence'_6 [] = return []
sequence'_6 (m : ms) = m >>= \a ->
  as <- sequence'_6 ms
  return (a : as)
--}

sequence'_7 :: Monad m => [m a] -> m [a]
sequence'_7 [] = return []
sequence'_7 (m : ms)
  = do a <- m
       as <- sequence'_7 ms
       return (a : as)

sequence' :: Monad m => [m a] -> m [a]
sequence' [] = return []
sequence' (m : ms)
  = do a <- m
       as <- sequence' ms
       return (a : as)

-- e7
e7_seq = ["1", "2", "3"]
mapM'_0 :: Monad m => (a -> m b) -> [a] -> m [b]
mapM'_0 f as = sequence' (map f as)

mapM'_1 :: Monad m => (a -> m b) -> [a] -> m [b]
mapM'_1 f [] = return []
mapM'_1 f (a : as)
  = f a >>= \b -> mapM'_1  f as >>= \bs -> return (b : bs)


{-- Fails with:
Couldn't match type ‘()’ with ‘[b]’
Expected type: m [b]
Actual type: m ()

mapM'_2 :: Monad m => (a -> m b) -> [a] -> m [b]
mapM'_2 f as = sequence_' (map f as)
--}


{-- Fails with:
Could not deduce (b ~ m0 [a0])
from the context (Monad m)
bound by the type signature for

mapM'_3 :: Monad m => (a -> m b) -> [a] -> m [b]
mapM'_3 :: Monad m => (a -> m b) -> [a] -> m [b]
mapM'_3 f [] = return []
mapM'_3 f (a : as)
  = f a >> \b -> mapM'_3  f as >> \bs -> return (b : bs)
--}

{-- Fails with: parse error on input ‘->’.
mapM'_4 :: Monad m => (a -> m b) -> [a] -> m [b]
mapM'_4 f [] = return []
mapM'_4 f (a : as) =
  do
    f a -> b
    mapM'_4  f as -> bs
    return (b : bs)
--}

mapM'_5 :: Monad m => (a -> m b) -> [a] -> m [b]
mapM'_5 f [] = return []
mapM'_5 f (a : as) =
  do
    b <- f a
    bs <- mapM'_5  f as
    return (b : bs)

mapM'_6 :: Monad m => (a -> m b) -> [a] -> m [b]
mapM'_6 f [] = return []
mapM'_6 f (a : as) =
  f a >>=
    \b ->
      do bs <- mapM'_6  f as
         return (b : bs)

mapM'_7 :: Monad m => (a -> m b) -> [a] -> m [b]
mapM'_7 f [] = return []
mapM'_7 f (a : as) =
  f a >>=
    \b ->
      do bs <- mapM'_7  f as
         return (bs ++ [b])

-- e8
-- Tested with: filterM' (\x -> return (even x)) [1 .. 10]
filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' _ [] = return []
filterM' p (x : xs)
  = do flag <- p x
       ys <- filterM' p xs
       if flag then return (x : ys) else return ys

