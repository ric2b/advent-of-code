import qualified Data.Set as S

import Data.List (elemIndices)

main = do
    raw_input <- lines <$> readFile "input/day17.txt"
    let initialWorldState = parseInitialSlice raw_input

    print $ S.size $ activePositions $ iterate tickWorldState initialWorldState !! 6 -- 348
    print $ S.size $ activePositions $ iterate tickWorldState (worldState3Dto4D initialWorldState) !! 6 -- 2236

data WorldState = WorldState { activePositions :: S.Set Position } deriving (Eq, Show)
data Position = Position3D { x::Int, y::Int, z::Int } | Position4D { x::Int, y::Int, z::Int, w::Int } deriving (Eq, Ord, Show)

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
neighbours Position3D{x=x, y=y, z=z} = S.fromList [Position3D{x=x+a, y=y+b, z=z+c} | a<-[-1..1], b<-[-1..1], c<-[-1..1], a /= 0 || b /= 0 || c /= 0]
neighbours Position4D{x=x, y=y, z=z, w=w} = S.fromList [Position4D{x=x+a, y=y+b, z=z+c, w=w+d} | a<-[-1..1], b<-[-1..1], c<-[-1..1], d<-[-1..1], a /= 0 || b /= 0 || c /= 0 || d /= 0]

worldState3Dto4D :: WorldState -> WorldState
worldState3Dto4D WorldState{activePositions=activePositions} = WorldState { activePositions = S.map position3Dto4D activePositions }
    where position3Dto4D Position3D{x=x, y=y, z=z} = Position4D{x=x, y=y, z=z, w=0}

parseInitialSlice :: [[Char]] -> WorldState
parseInitialSlice raw_input = WorldState {activePositions = activePositions}
    where activePositions = S.fromList $ concat [[Position3D{x=x, y=y, z=0} | x <- (elemIndices '#' row)] | (row, y) <- (zip raw_input [0..])]
