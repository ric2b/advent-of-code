#!/usr/bin/env runhaskell

import Data.List (\\)
import Data.Tuple (swap)
import qualified Data.Map as Map

main :: IO ()
main = do
    input <- readFile "input/day06.txt"
    print (part1 input)
    print (part2 exampleInput)

part1 = countOrbits . indirectOrbits . directOrbits . map parseOrbit . lines
    where countOrbits orbits = sum (map length (Map.elems orbits))

part2 = length . (shortestPath "K" "I") . directOrbits . map parseOrbit . lines

shortestPath :: Object -> Object -> Map.Map Object [Object] -> [Object]
shortestPath source destination directOrbits = bfs destination directOrbits [source] []

bfs :: Ord a => a -> Map.Map a [a] -> [a] -> [a] -> [a] -> [a]
bfs destination graph currentPath queue visited = case destination `elem` Map.findWithDefault [] (head queue) graph of
    True -> currentPath
    False -> bfs destination graph (source:currentPath) ((head queue):visited) (tail queue ++ unvisitedNeighbours)  
        where source = head queue 
              unvisitedNeigbours = Map.findWithDefault [] (head queue) graph \\ visited

{-
data BfsState = BfsState {
    source :: a,
    destination :: a,
    queue :: [a],
    currentPath :: [a]
} deriving (Eq)
-}
--bfs: 
--- start at source, check if goal reached, else add neighbours to stack
--- pop from stack, bfs 

buildGraph :: Ord a => [(a, a)] -> Map.Map a [a]
buildGraph connections = Map.fromListWith (++) (concatMap convertConnection connections)
    where convertConnection connection = [(fst connection, [snd connection]), (snd connection, [fst connection])]

indirectOrbits :: Map.Map Object [Object] -> Map.Map Object [Object]
indirectOrbits directOrbits = Map.mapWithKey (\object _ -> objectOrbitedBy directOrbits object) directOrbits

objectOrbitedBy :: Map.Map Object [Object] -> Object -> [Object]
objectOrbitedBy directOrbits object = case Map.findWithDefault [] object directOrbits of
    [] -> []
    orbits -> orbits ++ concatMap (objectOrbitedBy directOrbits) orbits

directOrbits :: [(Object, Object)] -> Map.Map Object [Object]
directOrbits orbits = Map.fromListWith (++) (map convertOrbit orbits)
    where convertOrbit orbit = (fst orbit, [snd orbit])

parseOrbit input = (fst objects, (drop 1 . snd) objects)
    where objects = break (==')') input

type Object = String

--[("B",["G","C"]),("C",["D"]),("COM",["B"]),("D",["I","E"]),("E",["J","F"]),("G",["H"]),("J",["K"]),("K",["L"])]
exampleInput = "COM)B\n\
                \B)C\n\
                \C)D\n\
                \D)E\n\
                \E)F\n\
                \B)G\n\
                \G)H\n\
                \D)I\n\
                \E)J\n\
                \J)K\n\
                \K)L"
