main = do
    input <- lines <$> readFile "input/day05.txt"
    let seatList = map seatId input
    print $ maximum seatList -- 965
    print $ findMySeat seatList -- 524

findMySeat :: [Int] -> Int
findMySeat seatList = head $ filter adjacentSeatsPresent $ filter (not . seatPresent) [0..maxId]
    where adjacentSeatsPresent seatId = seatPresent (seatId - 1) && seatPresent (seatId + 1)
          seatPresent seatId = seatId `elem` seatList
          maxId = 8 * (2^7 -1) + (2^3-1)

seatId code = (spacePartitioningRow code) * 8 + (spacePartitioningColumn code)

spacePartitioningRow code = toBinary 'B' (take 7 code)
spacePartitioningColumn code = toBinary 'R' (drop 7 code)

toBinary :: Char -> String -> Int
toBinary char1 code = sum $ map (\(i, b) -> b*2^i) withIndex
    where withIndex = zip [0..] (reverse binaryString)
          binaryString = map (\c -> if c == char1 then 1 else 0) code
