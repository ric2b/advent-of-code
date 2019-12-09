#!/usr/bin/env runhaskell

import Data.List (group)

import IntcodeComputer

main :: IO ()
main = do
    input <- readFile "input/day05.txt"
    print (part1 input)
    print (part2 input)

part1 input = executeProgram (loadState 1 input) 
part2 input = executeProgram (loadState 5 input) 

