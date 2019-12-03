import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe

data Point = Point {
    xCoordinate :: Int,
    yCoordinate :: Int
} deriving (Show, Ord, Eq)

manhattan :: Point -> Point -> Int
manhattan pointA pointB = 
    abs (xCoordinate pointA - xCoordinate pointB) + abs (yCoordinate pointA - yCoordinate pointB) 

parse :: String -> Point
parse coordinate = (\[x,y] -> Point x y) $ map read $ words $ filter (/= ',') coordinate

closest :: Point -> [Point] -> Maybe Point 
closest targetPoint points = fst $ foldr1 minDist [(Just point, manhattan point targetPoint) | point <- points]
    where minDist (point1, distance1) (point2, distance2)
            | distance1 == distance2 = (Nothing, maxBound)
            | distance1 < distance2 = (point1, distance1)
            | otherwise = (point2, distance2)
          

paintGrid maxX maxY points = [[closest (Point x y) points | x <- [0..maxX]] | y <- [0..maxY]]

infiniteAreas grid = Set.toList $
    Set.fromList (head grid) `Set.union` Set.fromList (last grid)
    `Set.union` Set.fromList (head tgrid) `Set.union` Set.fromList (last tgrid)
    where tgrid = transpose grid
    
mostFrequent :: (Ord a) => [a] -> (Maybe a, Int)
mostFrequent list = (Map.foldrWithKey folder (Nothing, 0) (frequencies list))
                where 
                    frequencies list = Map.fromListWith (+) [(item, 1) | item <- list]
                    folder key1 value1 item2 = if value1 >= snd item2 then (Just key1, value1) else item2

regionDistances :: Point -> [Point] -> Int
regionDistances regionCenter points = sum [manhattan regionCenter point | point <- points]  

validRegion :: Point -> [Point] -> Bool
validRegion regionCenter points = 10000 > regionDistances regionCenter points

eligibleRegions :: Int -> Int -> [Point] -> [Point]
eligibleRegions maxX maxY points = [Point x y | x <- [0..maxX], y <- [0..maxY], validRegion (Point x y) points]

main :: IO() 
main = do
    input <- readFile "input/day6.txt"
    let coordinates = map parse $ lines input
    let maxX = maximum $ map xCoordinate coordinates
    let maxY = maximum $ map yCoordinate coordinates
    let grid = paintGrid maxX maxY coordinates 
    let eligibleAreas = coordinates \\ catMaybes (infiniteAreas grid)
    print $ snd $ mostFrequent $ filter (`elem` eligibleAreas) $ catMaybes $ concat grid
    print $ length $ eligibleRegions maxX maxY coordinates
