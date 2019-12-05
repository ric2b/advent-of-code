#!/usr/bin/env runhaskell

import Data.Text (split, pack, unpack)
import Data.List (intersect, minimumBy, elemIndex)
import Data.Maybe (fromJust)
import qualified Data.Set as Set 

main :: IO ()
main = do
    input <- readFile "input/day03.txt"
    print (part1 input)
    print (part2 input)

part1 input = minimum (map distanceFromOrigin (foldl1 crossings wires))
    where wires = map (wirePath . vertices) (paths input)

part2 input = 2 + minimum (map (totalDelay wires) (foldl1 crossings wires))
    where wires = map (wirePath . vertices) (paths input)

totalDelay :: [WirePath] -> Point -> Int
totalDelay wirePaths point = sum $ map (wireDelay point) wirePaths

wireDelay :: Point -> WirePath -> Int
wireDelay point wirePath = fromJust (point `elemIndex` wirePath)

crossings :: WirePath -> WirePath -> [Point]
crossings visitedByA visitedByB = Set.toList (Set.intersection (Set.fromList visitedByA) (Set.fromList visitedByB))
--crossings visitedByA visitedByB = intersect visitedByA visitedByB -- somehow this is slower... 

wirePath :: [Point] -> WirePath
wirePath vertices = concatMap (tail . between) (zip vertices (tail vertices))

between :: (Point, Point) -> [Point]
between (pointA, pointB)
    | x pointA < x pointB = [pointA {x=newX} | newX <- [x pointA..x pointB]] 
    | x pointA > x pointB = [pointA {x=newX} | newX <- reverse [x pointB..x pointA]] 
    | y pointA < y pointB = [pointA {y=newY} | newY <- [y pointA..y pointB]] 
    | otherwise = [pointA {y=newY} | newY <- reverse [y pointB..y pointA]] 

vertices :: [String] -> [Point] 
vertices path = scanl movement (Point {x=0, y=0}) path 

movement :: Point -> String -> Point
movement point (direction:distance_str) 
    | direction == 'U' = point {y=(y point) + distance} 
    | direction == 'D' = point {y=(y point) - distance} 
    | direction == 'L' = point {x=(x point) - distance} 
    | direction == 'R' = point {x=(x point) + distance} 
        where distance = read distance_str

distanceFromOrigin point = manhattanDistance point (Point 0 0)

instance Distance Point where
    manhattanDistance pointA pointB = (abs (x pointA - x pointB)) + (abs (y pointA - y pointB))

class Distance point where
    manhattanDistance :: point -> point -> Integer

type WirePath = [Point]

data Point = Point {
    x :: Integer,
    y :: Integer
} deriving (Show, Eq, Ord)

paths input = map splitByComma (filter (not . null) (lines input))

splitByComma :: String -> [String]
splitByComma input = map unpack $ ((split (==',')) . pack . (filter (/= '\n'))) input

