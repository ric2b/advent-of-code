import qualified Data.Set as S

import Data.List (elemIndices)

main = do
    raw_input <- lines <$> readFile "input/day17.txt"
    let initialWorldState = parseInitialSlice raw_input

    print $ S.size $ activePositions $ iterate tickWorldState initialWorldState !! 6 -- 348


data WorldState = WorldState { activePositions :: S.Set Position } deriving (Eq, Show)

data Position = Position { x::Int, y::Int, z::Int } deriving (Eq, Ord, Show)

tickWorldState :: WorldState -> WorldState
tickWorldState worldState@(WorldState{activePositions=activePositions}) = worldState{activePositions=newActivePositions}
    where newActivePositions = S.filter (newPositionState worldState) (allRelevantPositions worldState)

newPositionState :: WorldState -> Position -> Bool
newPositionState worldState@(WorldState{activePositions=activePositions}) position = case position `S.member` activePositions of
    True -> activeNeighboursCount `elem` [2,3]
    False -> activeNeighboursCount == 3
    where activeNeighboursCount = S.size (activeNeighbours worldState position)

allRelevantPositions :: WorldState -> S.Set Position
allRelevantPositions WorldState{activePositions=activePositions} = S.foldr S.union activePositions (S.map neighbours activePositions)

activeNeighbours :: WorldState -> Position -> S.Set Position
activeNeighbours WorldState{activePositions=activePositions} position = S.intersection (neighbours position) activePositions

neighbours :: Position -> S.Set Position
neighbours Position{x=x, y=y, z=z} = S.fromList [Position{x=x+a, y=y+b, z=z+c} | a<-[-1..1], b<-[-1..1], c<-[-1..1], a /= 0 || b /= 0 || c /= 0]

parseInitialSlice :: [[Char]] -> WorldState
parseInitialSlice raw_input = WorldState {activePositions = activePositions}
    where activePositions = S.fromList $ concat [[Position{x=x, y=y, z=0} | x <- (elemIndices '#' row)] | (row, y) <- (zip raw_input [0..])]
