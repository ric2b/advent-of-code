import Data.List
import Data.Ord

gridSerialNumber = 6392

main :: IO() 
main = do
    print part1
    print part2

part1 = mostPowerfulSquare 300 3

part2 = maximumBy (comparing fst) [includePowerAndSize s (mostPowerfulSquare 300 s) | s <- [1..300]]
    where includePowerAndSize size position = (squarePowerLevel size position, (position, size))

data Position = Position {
    xPosition :: Int,
    yPosition :: Int
} deriving (Show)

rackId = (+10) . xPosition

powerLevel :: Position -> Int
powerLevel cell = (-5) + hundredsDigit ((gridSerialNumber + rackId cell * yPosition cell) * rackId cell)
    where hundredsDigit number = (number `quot` 100) `rem` 10

squarePowerLevel :: Int -> Position -> Int
squarePowerLevel size squarePosition = sum [powerLevel (Position (squareX + x) (squareY + y)) | x <- [0..size-1], y <- [0..size-1]]
    where 
        squareX = xPosition squarePosition
        squareY = yPosition squarePosition

mostPowerfulSquare :: Int -> Int -> Position
mostPowerfulSquare gridSize squareSize = 
    maximumBy (comparing (squarePowerLevel squareSize)) [Position x y | x <- [1..(gridSize-squareSize+1)], y <- [1..(gridSize-squareSize+1)]]