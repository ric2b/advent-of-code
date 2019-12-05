#!/usr/bin/env runhaskell

import Data.List (group)

main :: IO ()
main = do
    input <- readFile "input/day04.txt"
    print (part1 input)
    print (part2 input)

part1 input = length $ filter hasRepeat $ filter isSorted $ filter hasValidLength $ generateRange (parse input)

part2 input = length $ filter hasStrictRepeat $ filter isSorted $ filter hasValidLength $ generateRange (parse input)

hasStrictRepeat :: String -> Bool
hasStrictRepeat input = any (\digit -> length digit == 2) (group input)

hasRepeat :: String -> Bool
hasRepeat input = any (\digit -> length digit >= 2) (group input)

isSorted :: String -> Bool
isSorted input = all (\(a, b) -> a <= b) (zip input (tail input))

hasValidLength :: String -> Bool
hasValidLength = (==6) . length

generateRange (floor, ceiling) = map show [floor..ceiling]

parse :: String -> (Integer, Integer)  
parse input = case break (=='-') input of
    (floor, '-':ceiling) -> (read floor, (read . trimNewline) ceiling)

trimNewline = filter (/= '\n')

