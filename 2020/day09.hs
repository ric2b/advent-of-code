import Data.Maybe (fromJust)

main = do
    raw_input <- readFile "input/day09.txt"
    let input = map read $ lines raw_input :: [Int]
    let (preamble, rest) = splitAt 25 input
    let invalidNumber = fromJust $ findInvalidNumber preamble rest
    print invalidNumber -- 138879426
    print $ findContiguousParts invalidNumber [] input -- 23761694

findInvalidNumber :: (Num a, Eq a) => [a] -> [a] -> Maybe a
findInvalidNumber _ [] = Nothing
findInvalidNumber preamble (x:xs) = if isInvalid x then Just x else findInvalidNumber (tail preamble ++ [x]) xs
    where isInvalid x = not $ x `elem` [a + b | a <- preamble, b <- preamble, a /= b]

findContiguousParts :: (Num a, Eq a, Ord a) => a -> [a] -> [a] -> a
findContiguousParts target currentParts (next:rest)
    | currentSum == target = minimum currentParts + maximum currentParts
    | currentSum < target = findContiguousParts target (currentParts ++ [next]) rest
    | currentSum > target = findContiguousParts target (tail currentParts) (next:rest)
        where currentSum = sum currentParts
