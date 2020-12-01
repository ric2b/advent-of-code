main = do
    raw_input <- readFile "input/day01.txt"
    let input = map read (lines raw_input) :: [Int]
    let pairs = createGroups 2 input
    let triples = createGroups 3 input
    print $ calculate_expense pairs
    print $ calculate_expense triples

calculate_expense :: [[Int]] -> Int
calculate_expense = foldl1 (*) . head . filter valid_group

valid_group :: [Int] -> Bool
valid_group = (== 2020) . sum

createGroups :: Int -> [Int] -> [[Int]]
createGroups _ [] = []
createGroups 2 (x:xs) = map (\y -> [x, y]) xs ++ createGroups 2 xs
createGroups 3 (x:xs) = map (\y -> [x] ++ y) (createGroups 2 xs) ++ createGroups 3 xs
