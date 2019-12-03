#!/usr/bin/env runhaskell

main :: IO ()
main = do
    input <- readFile "input/day01.txt"
    print (part1 input)
    print (part2 input)

part1 input = (sum (map (fuelRequired . read) (lines input)))
part2 input = (sum (map (recursiveFuelRequired . read) (lines input)))

fuelRequired mass = mass `div` 3 - 2

recursiveFuelRequired mass 
    | fuelRequired mass <= 0 = 0
    | otherwise = (fuelRequired mass) + recursiveFuelRequired (fuelRequired mass)
