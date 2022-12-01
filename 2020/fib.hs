main = do
    print $ memoized_fib 30
    print $ memoized_fib2 30
    print $ open_memoized_fib 30

memoized_fib :: Int -> Integer
memoized_fib = (map fib [0..] !!)
    where fib 0 = 0
          fib 1 = 1
          fib n = memoized_fib(n - 2) + memoized_fib(n - 1)
 
memoized_fib2 :: Int -> Integer
memoized_fib2 x = (map fib[0..] !!) x
    where fib 0 = 0
          fib 1 = 1
          fib n = memoized_fib2(n - 2) + memoized_fib2(n - 1)

open_fib :: (Int -> Integer) -> Int -> Integer
open_fib _ 0 = 0
open_fib _ 1 = 1
open_fib mf n = mf(n - 2) + mf(n - 1)

open_memoized_fib :: Int -> Integer
open_memoized_fib = (map (open_fib open_memoized_fib) [0..] !!)

-- memoized_fib n = (map fib [0..]) !! n