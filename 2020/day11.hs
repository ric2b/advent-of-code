{-# LANGUAGE NamedFieldPuns #-}

import qualified Data.Map as M
import qualified Data.Set as S

import Data.List (find)
import Data.Maybe (catMaybes)

main = do
    raw_input <- lines <$> readFile "input/day11.txt"
    -- raw_input <- lines <$> readFile "input/day11.example.txt"
    let initialWaitingAreaState = parseWaitingArea raw_input
    -- -- mapM_ print raw_input
    -- -- print $ calculateSeats initialWaitingAreaState

    -- -- print $ occupiedSeats initialWaitingAreaState

    -- let areaStates = (iterate tickSeats initialWaitingAreaState !!)

    -- -- print $ areaStates 0
    -- print $ areaStates 1
    -- print $ areaStates 2
    -- print $ areaStates 3
    -- print $ areaStates 4
    -- print $ areaStates 5
    -- print $ areaStates 6

    -- print $ areaStates 5 == areaStates 6
    -- print $ tickSeats (areaStates 5) == areaStates 5
    
    print $ length . occupiedSeats $ untilSteadyState tickSeatsAdjacent initialWaitingAreaState -- 2263
    print $ length . occupiedSeats $ untilSteadyState tickSeatsSight initialWaitingAreaState -- 

type Position = (Int, Int)

data WaitingAreaState = WaitingAreaState {
        gridLimit :: Position,
        floorPositions :: S.Set Position,
        occupiedSeats :: S.Set Position
} deriving (Eq)

instance Show WaitingAreaState where
    show WaitingAreaState{gridLimit, floorPositions, occupiedSeats} = show [[
            c | x <- [0..maxX],
            let c = if S.member (x, y) floorPositions then '.' 
                else if S.member (x, y) occupiedSeats then '#' 
                else 'L'
        ] | y <- [0..maxY]]
        where (maxX, maxY) = gridLimit

-- createWaitingAreaState gridLimit floorPositions occupiedSeats = 
--     WaitingAreaState {gridLimit = (maxX, maxY), floorPositions, occupiedSeats, seats}
--     where seats = S.fromList [(x, y) | y <- [0..maxY], x <- [0..maxX], S.notMember (x, y) floorPositions]
--     where (maxX, maxY) = gridLimit 

untilSteadyState tickSeats currentState = case currentState == nextState of 
    True -> currentState
    False -> untilSteadyState tickSeats nextState
    where nextState = tickSeats currentState

tickSeatsSight :: WaitingAreaState -> WaitingAreaState
tickSeatsSight currentState = currentState{occupiedSeats=newOccupiedSeats}
    where newOccupiedSeats = S.filter (becomesOccupiedSight currentState) seats
          seats = calculateSeats currentState

becomesOccupiedSight :: WaitingAreaState -> Position -> Bool
becomesOccupiedSight waitingAreaState@(WaitingAreaState{occupiedSeats}) (x, y) = case (x, y) `S.member` occupiedSeats of
    False -> occupiedNeighboursCount == 0
    True -> occupiedNeighboursCount < 5
    where occupiedNeighboursCount = length $ filter (`S.member` occupiedSeats) neighbouringSeats
          neighbouringSeats = neighboursBySight waitingAreaState (x, y)
        --   allOcupiedSeats = occupiedSeats waitingAreaState


neighboursBySight waitingAreaState candidateSeat = catMaybes $ map findFirstSeat seatSightLines
    where findFirstSeat line = find (`S.member` allSeats) line
          seatSightLines = centerSightLines waitingAreaState candidateSeat
          allSeats = calculateSeats waitingAreaState

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
tickSeatsAdjacent currentState = currentState{occupiedSeats=newOccupiedSeats}
    where newOccupiedSeats = S.filter (becomesOccupiedAdjacent currentState) seats
          seats = calculateSeats currentState

becomesOccupiedAdjacent :: WaitingAreaState -> Position -> Bool
becomesOccupiedAdjacent WaitingAreaState{occupiedSeats} (x, y) = case (x, y) `S.member` occupiedSeats of
    False -> occupiedNeighboursCount == 0
    True -> occupiedNeighboursCount < 4
    where occupiedNeighboursCount = length $ filter (`S.member` occupiedSeats) neighbouringSeats
          neighbouringSeats = [(x+a, y+b) | b <- [-1,0,1], a <- [-1,0,1], a /= 0 || b /= 0]

calculateSeats :: WaitingAreaState -> S.Set Position
calculateSeats WaitingAreaState{gridLimit, floorPositions} = 
    S.fromList [(x, y) | y <- [0..maxY], x <- [0..maxX], S.notMember (x, y) floorPositions]
        where (maxX, maxY) = gridLimit

-- loading map

parseWaitingArea :: [[Char]] -> WaitingAreaState
parseWaitingArea raw_input = WaitingAreaState {gridLimit = (maxX, maxY), floorPositions, occupiedSeats}
    where (maxX, maxY) = ((length . head) raw_input - 1 , (length raw_input) - 1)
          floorPositions = S.fromList $ positionsWith '.' raw_input
          occupiedSeats = S.fromList $ positionsWith '#' raw_input

positionsWith :: Char -> [[Char]] -> [(Int, Int)]
positionsWith char raw_input = concat [[(x, y) | (c, x) <- (zip row [0..]), c == char] | (row, y) <- (zip raw_input [0..])]
