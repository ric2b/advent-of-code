import Data.Maybe (fromJust) 
import Data.List (sort, mapAccumL, tails, elemIndex)
import qualified Data.Map as Map

import Data.Function (fix)

import Debug.Trace

main = do
    raw_input <- readFile "input/day10.txt"
    let adapters = map read $ lines raw_input :: [Int]
    let deviceJolts = (maximum adapters) + 3
    let joltDifferences = snd $ mapAccumL (\prev curr -> (curr, curr - prev)) 0 (sort (deviceJolts:adapters))
    print $ (length . filter (1==)) joltDifferences * (length . filter (3==)) joltDifferences -- 1904

    -- print $ validImmediateJumpCount [5]
    -- print $ validImmediateJumpCount [4,5]
    -- print $ validImmediateJumpCount [2,4,5]
    -- print $ validImmediateJumpCount [1,2,4,5]
    -- print $ validImmediateJumpCount [0,1,2,4,5]

    let sorted = sort adapters
    let x = map possibleArrangements $ tails sorted
    let eindex = fromJust $ 142 `elemIndex` sorted
    -- print $ dropWhile (< 142) sorted
    -- print $ x !! eindex
    -- print $ possibleArrangements [5]
    -- print $ possibleArrangements [4,5]
    -- print $ possibleArrangements [2,4,5]
    -- print $ possibleArrangements [1,2,4,5]
    -- print $ possibleArrangements [0,1,2,4,5]
    -- print $ possibleArrangements Map.empty $ sort [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4]
    print $ possibleArrangements $ sort [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4]
    -- print $ fix openPossibleArrangements $ sort [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4]
    print $ memoizedPossibleArrangements (sort [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4]) 1
    -- print $ memoizedOpenPossibleArrangements (sort [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4]) 1

    print $ part2 (sort [16, 10, 15, 5, 11, 7, 19, 6, 12, 4]) [(1, 1)]

    print $ possibleArrangementsCount (sort [16, 10, 15, 5, 11, 7, 19, 6, 12, 4]) []
    -- print $ memoizedPossibleArrangements (sort [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4]) 1
    print $ possibleArrangements $ sort [0,1,2,4,5]
    -- print $ fix openPossibleArrangements $ sort [0,1,2,4,5]
    print $ memoizedPossibleArrangements (sort [0,1,2,4,5]) 0
    -- print $ memoizedOpenPossibleArrangements (sort [0,1,2,4,5]) 0
  
    -- print $ tails $ sort [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4]
    -- print $ fromJust $ 7 `elemIndex` (sort [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4])

    -- print $ possibleArrangements $ sort (0:deviceJolts:adapters)
    -- print $ memoizedPossibleArrangements (sort (0:deviceJolts:adapters)) 0
    print $ part2 (sort adapters) [(0, 1)]

    print $ part2 [1,2,4,5] [(0, 1)]
    print $ possibleArrangementsCount [1,2,4,5] [(0,1)]
    print $ possibleArrangementsCount [0,1,2,4,5] []

    print $ possibleArrangementsCount (sort (0:adapters)) [] -- 10578455953408
    

-- part2 (x:xs) memo | trace (show (x:xs) ++ show memo)  False = undefined
part2 (currentAdapter:rest) memo = case rest of
    [] -> count
    _  -> part2 rest ((currentAdapter, count):memo)
    where count = sum . map snd $ takeWhile ((currentAdapter - 3 <=) . fst ) memo
    -- where i = sum . map snd $ takeWhile (\(y, _) -> x - y <= 3) memo

possibleArrangementsCount :: [Int] -> [(Int, Int)] -> Int
possibleArrangementsCount (currentAdapter:adapters) [] = possibleArrangementsCount adapters [(currentAdapter, 1)]
possibleArrangementsCount (currentAdapter:adapters) memory = case adapters of
    [] -> count
    _ -> possibleArrangementsCount adapters ((currentAdapter, count):memory)
    where count = sum . map snd $ takeWhile ((>= currentAdapter-3) . fst) memory

-- adapters must be sorted
-- possibleArrangements :: (Num a, Ord a) => [a] -> Int
-- possibleArrangements [] = 0
-- possibleArrangements (currentAdapter:[]) = 1
-- possibleArrangements adapters = sum $ snd $ mapAccumL possibleArrangementsFrom Map.empty $ nextValidAdapters adapters
--     where possibleArrangementsFrom memory nextAdapter = case (Map.lookup nextAdapter memory) of
--             (Just arrangementCount) -> (memory, arrangementCount)
--             Nothing -> (Map.insert nextAdapter arrangementCount memory, arrangementCount)
--                         where arrangementCount = possibleArrangements $ dropWhile (< nextAdapter) adapters 

        
-- memoized_fib n = (map fib [0..]) !! n

-- memoized_fib :: Int -> Integer
-- memoized_fib = (map fib [0 ..] !!)
--    where fib 0 = 0
--          fib 1 = 1
--          fib n = memoized_fib (n-2) + memoized_fib (n-1)

-- memoizedPossibleArrangements adapters currentAdapter | trace (show adapters ++ show currentAdapter ++ show (dropWhile (< currentAdapter) adapters))  False = undefined

memoizedPossibleArrangements adapters currentAdapter = (map (possibleArrangements2 adapters) adapters) !! adapterIndex
    where adapterIndex = fromJust $ currentAdapter `elemIndex` adapters
          possibleArrangements2 adapters currentAdapter = case (dropWhile (< currentAdapter) adapters) of
            [] -> 0
            (currentAdapter:[]) -> 1
            adapters -> sum $ map possibleArrangementsFrom $ nextValidAdapters (dropWhile (< currentAdapter) adapters)
                where possibleArrangementsFrom nextAdapter = memoizedPossibleArrangements adapters nextAdapter

-- memoizedOpenPossibleArrangements :: (Num a, Ord a) => [a] -> Int -> Int
-- memoizedOpenPossibleArrangements adapters = (map (openPossibleArrangements (memoizedOpenPossibleArrangements adapters) adapters) adapters !!)                

-- openPossibleArrangements :: (Num a, Ord a) => ([a] -> Int) -> [a] -> Int -> Int
-- openPossibleArrangements _ [] _ = 0
-- openPossibleArrangements _ (currentAdapter:[]) _ = 1
-- openPossibleArrangements mf adapters = sum $ map possibleArrangementsFrom $ nextValidAdapters adapters
--     where possibleArrangementsFrom nextAdapter = mf adapters (fromJust $ nextAdapter `elemIndex` adapters)
                
                
            
possibleArrangements :: (Num a, Ord a) => [a] -> Int
possibleArrangements [] = 0
possibleArrangements (currentAdapter:[]) = 1
possibleArrangements adapters = sum $ map possibleArrangementsFrom $ nextValidAdapters adapters
    where possibleArrangementsFrom nextAdapter = possibleArrangements $ dropWhile (< nextAdapter) adapters


-- possibleArrangements :: (Num a, Ord a) => [a] -> [[a]]
-- possibleArrangements (currentAdapter:[]) = [[currentAdapter]]
-- possibleArrangements (currentAdapter:nextAdapters) = map (\xs -> currentAdapter:xs) $ concatMap possibleArrangementsFrom (nextValidAdapters (currentAdapter:nextAdapters))
--     where possibleArrangementsFrom adapter = possibleArrangements (adapter:(dropWhile (<= adapter) nextAdapters))

-- memoPossibleArrangementsCount adapters = sum $ map snd $ mapAccumL possibleArrangementsFrom Map.empty adapters
--     where possibleArrangementsFrom memory nextAdapter = case Map.lookup nextAdapter memory of
--         (Just arrangementCount) = (memory, arrangementCount)
--         Nothing -> (Map.insert adapter arrangementCount memory, arrangementCount)
--             where arrangementCount = possibleArrangements (adapter:(dropWhile (<= adapter) nextAdapters))


validImmediateJumpCount = length . nextValidAdapters

-- adapters must be sorted 
nextValidAdapters :: (Num a, Ord a) => [a] -> [a]
nextValidAdapters (currentAdapter:rest) = takeWhile (<= currentAdapter + 3) rest
