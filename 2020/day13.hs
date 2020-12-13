import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.Maybe (isJust, fromJust, catMaybes)
import Text.Read (readMaybe)

import qualified Data.Map as M

main = do
    rawArrivalTime:rawBusList:[] <- lines <$> readFile "input/day13.txt"
    let arrivalTime = read rawArrivalTime :: Int
    let busList = map readMaybe $ splitOn ',' rawBusList :: [Maybe Int]
    let workingBuses = catMaybes busList
    
    print $ (\(id, earliestTime) -> id * (earliestTime - arrivalTime)) $ earliestBus workingBuses arrivalTime -- 246
    print $ snd $ foldl1 combineBusCycles $ map (\(v, i) -> (fromJust v, i)) $ filter (isJust . fst) $ zip busList [0..] -- 939490236001473

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
