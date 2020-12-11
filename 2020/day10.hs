import Data.List (sort, mapAccumL)

main = do
    raw_input <- readFile "input/day10.txt"
    let adapters = map read $ lines raw_input :: [Int]
    let deviceJolts = (maximum adapters) + 3
    let joltDifferences = snd $ mapAccumL (\prev curr -> (curr, curr - prev)) 0 (sort (deviceJolts:adapters))

    print $ (length . filter (1==)) joltDifferences * (length . filter (3==)) joltDifferences -- 1904
    print $ possibleArrangementsCount (sort (0:deviceJolts:adapters)) [] -- 10578455953408

possibleArrangementsCount :: [Int] -> [(Int, Int)] -> Int
possibleArrangementsCount (currentAdapter:adapters) [] = possibleArrangementsCount adapters [(currentAdapter, 1)]
possibleArrangementsCount (currentAdapter:adapters) memory = case adapters of
    [] -> count
    _ -> possibleArrangementsCount adapters ((currentAdapter, count):memory)
    where count = sum . map snd $ takeWhile ((>= currentAdapter-3) . fst) memory
