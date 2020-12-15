import qualified Data.Map as M

main = do
    let startingNumbers = [0,8,15,2,12,1,4]
    
    let startingIndex = (last startingNumbers, length startingNumbers, M.fromList $ zip (init startingNumbers) [1..])

    print $ getNumberAtRound 2020 startingIndex -- 289
    print $ getNumberAtRound 30000000 startingIndex -- 1505722

getNumberAtRound :: Int -> (Int, Int, M.Map Int Int) -> Int
getNumberAtRound round (lastNumber, lastRound, memory)
    | round == lastRound = lastNumber
    | otherwise = getNumberAtRound round (sayNextNumber (lastNumber, lastRound, memory))

sayNextNumber :: (Int, Int, M.Map Int Int) -> (Int, Int, M.Map Int Int)
sayNextNumber (lastNumber, lastRound, memory) = case M.lookup lastNumber memory of
    Nothing -> (0, currentRound, M.insert lastNumber lastRound memory)
    (Just lastSpokenOnRound) -> (lastRound - lastSpokenOnRound, currentRound, M.insert lastNumber lastRound memory)
    where currentRound = lastRound + 1
