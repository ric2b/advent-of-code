import Data.List (minimumBy, find)
import Data.Ord (comparing)
import Data.Maybe (isJust, fromJust, catMaybes)
import Text.Read (readMaybe)

import qualified Data.Map as M

main = do
    rawArrivalTime:rawBusList:[] <- lines <$> readFile "input/day13.txt"
    -- rawArrivalTime:rawBusList:[] <- lines <$> readFile "input/day13.example.txt"
    let arrivalTime = read rawArrivalTime :: Int
    let busList = map readMaybe $ splitOn ',' rawBusList :: [Maybe Int]
    let workingBuses = catMaybes busList
    
    print $ (\(id, earliestTime) -> id * (earliestTime - arrivalTime)) $ earliestBus workingBuses arrivalTime -- 246
    print $ snd $ foldl1 combineBusCycles $ map (\(v, i) -> (fromJust v, i)) $ filter (isJust . fst) $ zip busList [0..] -- 939490236001473

    -- 0  2 3 5
    -- 1  
    -- 2  2
    -- 3    3
    -- 4  2
    -- 5      5
    -- 6  2 3
    -- 7
    -- 8  2
    -- 9    3
    -- 10 2   5
    -- 11 
    -- 12 2 3
    -- 13
    -- 14 2
    -- 15   3 5
    -- 16 2
    -- 17 
    -- 18 2 3
    -- 19 
    -- 20 2   5
    -- 21   3
    -- 22 2
    -- 23
    -- 24 2 3
    -- 25     5
    -- 26 2
    -- 27   3
    -- 28 2
    -- 29
    -- 30 2 3 5
    -- 31
    -- 32 2
    -- 33   3 
    -- 34 2
    -- 35     5
    -- 36 2 3
    -- 37
    -- 38 2   
    -- 39   3 
    -- 40 2   5

combineBusCycles :: (Int, Int) -> (Int, Int) -> (Int, Int)
combineBusCycles (busIdA, offsetA) (busIdB, offsetB) = (period, first)
    where period = busIdA * busIdB
          first = head [ t | t <- [offsetA, offsetA+busIdA..], ((t + offsetB) `rem` busIdB) == 0]

earliestBus :: [Int] -> Int -> (Int, Int)
earliestBus busIds startingAt = minimumBy (comparing snd) $ M.toList earliestViableBusTimes
    where earliestViableBusTimes = M.map head $ M.filter (not . null) viableBusTimes
          viableBusTimes = M.map (dropWhile (< startingAt)) busSchedules
          busSchedules = M.fromList $ zip busIds (map busTimes busIds)

busTimes :: Int -> [Int]
busTimes interval = iterate (+interval) 0

splitOn delimiter xs = case break (== delimiter) xs of
    (y, _ : ys) -> y : splitOn delimiter ys
    (y, []) -> [y]
