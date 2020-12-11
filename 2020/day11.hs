{-# LANGUAGE NamedFieldPuns #-}

import qualified Data.Map as M

import Data.List (find)
import Data.Maybe (catMaybes)

main = do
    raw_input <- lines <$> readFile "input/day11.txt"
    let initialWaitingAreaState = parseWaitingArea raw_input
    
    print $ length $ M.filter (==True) $ seats $ untilSteadyState tickSeatsAdjacent initialWaitingAreaState -- 2263
    print $ length $ M.filter (==True) $ seats $ untilSteadyState tickSeatsSight initialWaitingAreaState -- 2002

type Position = (Int, Int)

data WaitingAreaState = WaitingAreaState {
        gridLimit :: Position,
        seats :: M.Map Position Bool
} deriving (Eq)

instance Show WaitingAreaState where
    show WaitingAreaState{gridLimit, seats} = show [[
            c | x <- [0..maxX],
            let c = case M.lookup (x, y) seats of 
                        Nothing -> '.' 
                        (Just True) -> '#' 
                        (Just False) -> 'L'
        ] | y <- [0..maxY]]
        where (maxX, maxY) = gridLimit

untilSteadyState tickSeats currentState = case currentState == nextState of 
    True -> currentState
    False -> untilSteadyState tickSeats nextState
    where nextState = tickSeats currentState

tickSeatsSight :: WaitingAreaState -> WaitingAreaState
tickSeatsSight currentState@(WaitingAreaState{seats}) = currentState{seats=newSeatStates}
    where newSeatStates = M.mapWithKey updateSeat seats
          updateSeat (x, y) occupied = case occupied of
                False -> occupiedNeighboursCount == 0
                True -> occupiedNeighboursCount < 5
            where occupiedNeighboursCount = length neighbouringSeats
                  neighbouringSeats = [
                      neighbourOccupied | p <- (neighboursBySight currentState (x, y)),
                        let neighbourOccupied = case M.lookup p seats of
                                Nothing -> False
                                (Just o) -> o,
                        neighbourOccupied
                    ]

neighboursBySight waitingAreaState@(WaitingAreaState{seats}) candidateSeat = catMaybes $ map findFirstSeat seatSightLines
    where findFirstSeat line = find (`M.member` seats) line
          seatSightLines = centerSightLines waitingAreaState candidateSeat

centerSightLines waitingAreaState candidateSeat = map (boundToGrid waitingAreaState . centerLine candidateSeat) neighboursSightLines

boundToGrid WaitingAreaState{gridLimit} line = takeWhile (\(x, y) -> x >= 0 && y >= 0 && x <= maxX && y <= maxY) line
    where (maxX, maxY) = gridLimit

centerLine (x, y) line = map (\(a, b) -> (x+a, y+b)) line    

neighboursSightLines = [
        [(-a, -a) | a <- [1..]],
        [(0, -a) | a <- [1..]],
        [(a, -a) | a <- [1..]],
        [(-a, 0) | a <- [1..]],
        [(a, 0) | a <- [1..]],
        [(-a, a) | a <- [1..]],
        [(0, a) | a <- [1..]],
        [(a, a) | a <- [1..]]
    ]

tickSeatsAdjacent :: WaitingAreaState -> WaitingAreaState
tickSeatsAdjacent currentState@(WaitingAreaState{seats}) = currentState{seats=newSeatStates}
    where newSeatStates = M.mapWithKey updateSeat seats
          updateSeat (x, y) occupied = case occupied of
                False -> occupiedNeighboursCount == 0
                True -> occupiedNeighboursCount < 4
            where occupiedNeighboursCount = length neighbouringSeats
                  neighbouringSeats = [
                      neighbourOccupied | b <- [-1,0,1], a <- [-1,0,1], 
                        let neighbourOccupied = case M.lookup (x+a, y+b) seats of
                                Nothing -> False
                                (Just o) -> o,
                        neighbourOccupied,
                        a /= 0 || b /= 0
                    ]

-- loading map

parseWaitingArea :: [[Char]] -> WaitingAreaState
parseWaitingArea raw_input = WaitingAreaState {gridLimit = (maxX, maxY), seats = M.union freeSeats occupiedSeats}
    where (maxX, maxY) = ((length . head) raw_input - 1 , (length raw_input) - 1)
          freeSeats = M.fromList $ zip (positionsWith 'L' raw_input) (repeat False)
          occupiedSeats = M.fromList $ zip (positionsWith '#' raw_input) (repeat True)

positionsWith :: Char -> [[Char]] -> [(Int, Int)]
positionsWith char raw_input = concat [[(x, y) | (c, x) <- (zip row [0..]), c == char] | (row, y) <- (zip raw_input [0..])]
