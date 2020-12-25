import Data.List (find)
import Data.Maybe (fromJust)

main = do
    (publicKeyA:publicKeyB:[]) <- map read <$> lines <$> readFile "input/day25.txt"
    -- (publicKeyA:publicKeyB:[]) <- map read <$> lines <$> readFile "input/day25.example.txt"
    -- print publicKeyA
    -- print publicKeyB
    -- print $ publicKeyA + publicKeyB
    -- print $ transform 7 8
    -- let loopSizeA = breakLoopNumber publicKeyA
    -- let loopSizeB = breakLoopNumber publicKeyB
    -- let encryptionKeyA = transform publicKeyA loopSizeB
    -- let encryptionKeyB = transform publicKeyB loopSizeA
    -- print encryptionKeyA
    -- print encryptionKeyB
    -- print $ memoizedTransfrom 7 10000000
    -- print $ memoizedTransfrom 7 10000001
    -- print $ memoizedTransfrom 7 10000002
    -- print $ memoizedTransfrom 7 10000003
    -- print $ memoizedTransfrom 7 10000004

    let loopSizeA = fastBreakLoopNumber publicKeyA (1, 0)
    let loopSizeB = fastBreakLoopNumber publicKeyB (1, 0)
    let encryptionKeyA = transform publicKeyA loopSizeB
    let encryptionKeyB = transform publicKeyB loopSizeA
    print encryptionKeyA -- 6011069
    print encryptionKeyB -- 6011069
    -- print $ breakLoopNumber publicKeyA

-- diffieHellman :: 
-- diffieHellman

breakLoopNumber :: Int -> Int
breakLoopNumber publicKey = fromJust $ find ((== publicKey) . (transform 7)) [1..]

-- memoized_fib :: Int -> Integer
-- memoized_fib = (map fib [0 ..] !!)
--    where fib 0 = 0
--          fib 1 = 1
--          fib n = memoized_fib (n-2) + memoized_fib (n-1)

fastBreakLoopNumber :: Int -> (Int, Int) -> Int
fastBreakLoopNumber publicKey (currentValue, loopNumber)
    | publicKey == currentValue = loopNumber
    | otherwise = fastBreakLoopNumber publicKey (currentValue * 7 `mod` 20201227, loopNumber + 1)

transform :: Int -> Int -> Int
-- transform subjectNumber = (iterate (\value -> value * subjectNumber `mod` 20201227) 1 !!)
transform subjectNumber loops = iterate (\value -> value * subjectNumber `mod` 20201227) 1 !! loops
