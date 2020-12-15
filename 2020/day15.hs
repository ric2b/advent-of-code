import Text.ParserCombinators.Parsec

import qualified Data.Map as M

main = do
    rawInput <- readFile "input/day15.txt"
    let (Right startingNumbers) = parse parseNumbers "" rawInput
    
    let memory = M.fromList $ zip (reverse $ tail $ reverse startingNumbers) [1..]
    let startingIndex = (head $ reverse startingNumbers, length startingNumbers, memory)

    print $ getNumberAtRound 2020 startingIndex -- 289
    print $ getNumberAtRound 30000000 startingIndex -- 1505722

getNumberAtRound :: Int -> (Int, Int, M.Map Int Int) -> Int
getNumberAtRound round (lastNumber, lastRound, memory)
    | round == lastRound = lastNumber
    | otherwise = getNumberAtRound round (sayNextNumberFast (lastNumber, lastRound, memory))

sayNextNumberFast :: (Int, Int, M.Map Int Int) -> (Int, Int, M.Map Int Int)
sayNextNumberFast (lastNumber, lastRound, memory) = case M.lookup lastNumber memory of
    Nothing -> (0, currentRound, M.insert lastNumber lastRound memory)
    (Just lastSpokenOnRound) -> (lastRound - lastSpokenOnRound, currentRound, M.insert lastNumber lastRound memory)
    where currentRound = lastRound + 1

parseNumbers :: Parser [Int]
parseNumbers = do
    x <- read <$> many1 digit
    optional (char ',')
    xs <- option [] parseNumbers
    return (x:xs)
