module Lab5 where

import Control.Monad

data Concurrent a = Concurrent ((a -> Action) -> Action)

data Action
    = Atom (IO Action)
    | Fork Action Action
    | Stop

instance Show Action where
    show (Atom x) = "atom"
    show (Fork x y) = "fork " ++ show x ++ " " ++ show y
    show Stop = "stop"


-- ===================================
-- Ex. 0
-- ===================================

action' :: ((a -> Action) -> Action) -> Action
action' f =  f (\x -> Stop)

action :: Concurrent a -> Action
action (Concurrent f) = f (\x -> Stop)


-- ===================================
-- Ex. 1
-- ===================================
stop' :: (a -> Action) -> Action
stop' = \c -> Stop

stop :: Concurrent a
stop = Concurrent (\c -> Stop)


-- ===================================
-- Ex. 2
-- ===================================
atom' :: IO a -> ((a -> Action) -> Action)
atom' a c = Atom (a >>= \x -> return (c x))

--atom'' :: IO a -> ((a -> Action) -> Action)
--atom'' a c = (a >>= \x -> c) (\io_action -> Atom io_action)

atom :: IO a -> Concurrent a
atom a = Concurrent f
  where f c = Atom (a >>= \x -> return (c x))


-- ===================================
-- Ex. 3
-- ===================================

fork :: Concurrent a -> Concurrent ()
fork (Concurrent f) = Concurrent f
  where
    f c = Fork (action (Concurrent f)) (c ())

par :: Concurrent a -> Concurrent a -> Concurrent a
par a b = Concurrent f
  where
    fa = fork a
    fb = fork b
    f c = Fork (action fa) (action fb)
  {-where-}
    {-f c = Fork (action a) (action b)-}

e1 = do fork (atom $ putStrLn "test")
        atom $ putStrLn "hello"

e2 = do val <- par (atom $ return "hi") (atom $ return "hello")
        atom $ putStrLn val

-- ===================================
-- Ex. 4
-- ===================================

ma :: ((a -> Action) -> Action)
ma = error "Stub ma."

f :: a -> ((b -> Action) -> Action)
f = error "Stub f."

bind :: ((a -> Action) -> Action) -> (a -> ((b -> Action) -> Action)) -> ((b -> Action) -> Action)
--bind ma f =  (\c -> (f ma) c)
bind ma f =  (\c -> (ma (\x -> (f x) c)))

bind1 :: Concurrent a -> (a -> ((b -> Action) -> Action)) -> ((b -> Action) -> Action)
bind1 (Concurrent ma) f =  (\c -> (ma (\x -> (f x) c)))

bind2 :: Concurrent a -> (a -> Concurrent b) -> ((b -> Action) -> Action)
bind2 (Concurrent ma) f =  (\c -> (ma (\x -> case (f x) of (Concurrent b) -> b c)))

bind3 :: Concurrent a -> (a -> Concurrent b) -> Concurrent b
bind3 (Concurrent ma) f =  Concurrent (\c -> (ma (\x -> case (f x) of (Concurrent b) -> b c)))

instance Monad Concurrent where
   -- (Concurrent f) >>= g = error "You have to implement >>="
    (Concurrent f) >>= g = Concurrent(\c -> (f (\x -> case (g x) of (Concurrent b) -> b c)))
    return x = Concurrent (\c -> c x)




-- ===================================
-- Ex. 5
-- ===================================

roundRobin :: [Action] -> IO ()
roundRobin = error "You have to implement roundRobin"

-- ===================================
-- Tests
-- ===================================

ex0 :: Concurrent ()
ex0 = par (loop (genRandom 1337)) (loop (genRandom 2600) >> atom (putStrLn ""))

ex1 :: Concurrent ()
ex1 = do atom (putStr "Haskell")
         fork (loop $ genRandom 7331)
         loop $ genRandom 42
         atom (putStrLn "")


-- ===================================
-- Helper Functions
-- ===================================

run :: Concurrent a -> IO ()
run x = roundRobin [action x]

genRandom :: Int -> [Int]
genRandom 1337 = [1, 96, 36, 11, 42, 47, 9, 1, 62, 73]
genRandom 7331 = [17, 73, 92, 36, 22, 72, 19, 35, 6, 74]
genRandom 2600 = [83, 98, 35, 84, 44, 61, 54, 35, 83, 9]
genRandom 42   = [71, 71, 17, 14, 16, 91, 18, 71, 58, 75]

loop :: [Int] -> Concurrent ()
loop xs = mapM_ (atom . putStr . show) xs



add_cps :: Int -> Int -> ((Int -> r) -> r)
add_cps x y = (\k -> k (x + y))

add_cps' :: Int -> Int -> ((Int -> r) -> r)
add_cps' x y = (\m -> (\l -> (\k -> k (x + y)) l) m)

square_cps :: Int -> ((Int -> r) -> r)
square_cps x = \k -> k (x * x)


pythagoras_cps :: Int -> Int -> ((Int -> r) -> r)
pythagoras_cps x y =
  (\k ->
    ((\a b -> b (a * a)) x) (\x_squared ->
      ((\a b -> b (a * a)) y) (\y_squared ->
        ((\a b c -> c (a + b)) x_squared y_squared)  k)))

pythagoras_cps' :: Int -> Int -> ((Int -> r) -> r)
pythagoras_cps' x y =
  (\k ->
    ((\a b c -> c (a + b)) (x * x) (y * y))  k)

pythagoras_cps'' x y =
    ((\a b -> b (a * a)) x) (\x_squared ->
      ((\a b -> b (a * a)) y) (\y_squared ->
         ((\a b c -> c (a + b)) x_squared y_squared)))

pythagoras_cps''' :: Int -> Int -> ((Int -> r) -> r)
pythagoras_cps''' x y =
  \k ->
    (\a b -> b (a * a)) x $ \x_squared ->
                              (\a b -> b (a * a)) y $ \y_squared ->
                                                       (\a b c -> c (a + b)) x_squared y_squared k

pythagoras_cps'''' :: Int -> Int -> ((Int -> r) -> r)
pythagoras_cps'''' x y =
    (\a b -> b (a * a)) x $ \x_squared ->
                              (\a b -> b (a * a)) y $ \y_squared ->
                                                       (\a b c -> c (a + b)) x_squared y_squared
