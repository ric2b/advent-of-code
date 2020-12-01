main = do
    raw_input <- readFile "input/day01.txt"
    let input = map read (lines raw_input) :: [Int]
    print $ calculate_expense $ createGroups 2 input -- 357504
    print $ calculate_expense $ createGroups 3 input -- 12747392

calculate_expense :: [[Int]] -> Int
calculate_expense = foldl1 (*) . head . filter valid_group

valid_group :: [Int] -> Bool
valid_group = (== 2020) . sum

createGroups :: Int -> [Int] -> [[Int]]
createGroups _ [] = []
createGroups 2 (x:xs) = map (\y -> [x, y]) xs ++ createGroups 2 xs
createGroups size (x:xs) = map (\y -> [x] ++ y) (createGroups (size-1) xs) ++ createGroups size xs

