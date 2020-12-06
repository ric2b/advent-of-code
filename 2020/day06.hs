import Data.List

main = do
    input <- lines <$> readFile "input/day06.txt"
    let groups = splitOn "" input
    print $ sum $ map (length . foldl1 union) groups -- 6947
    print $ sum $ map (length . foldl1 intersect) groups -- 3398

splitOn delimiter xs = case break (== delimiter) xs of
    (y, _ : ys) -> y : splitOn delimiter ys
    (y, []) -> [y]
