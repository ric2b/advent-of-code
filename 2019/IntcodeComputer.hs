#!/usr/bin/env runhaskell

module IntcodeComputer (
    executeProgram,
    loadState
) where

import Data.Text (split, pack, unpack)
import qualified Data.Sequence as Seq

loadState :: Int -> String -> ComputerState
loadState input memory = ComputerState input 0 (Seq.fromList intList) 0
    where intList = map (read . unpack) splitByComma
            where splitByComma = split (==',') (pack (filter (/= '\n') memory))

executeProgram :: ComputerState -> Int
executeProgram state = case isExit (readAddress (memory state) (instructionPointer state)) of
    True -> output state
    False -> executeProgram updatedState
                where updatedState = executeOperation state

executeOperation :: ComputerState -> ComputerState
executeOperation state = operation state
    where operation = opcodeToOperation (readAddress (memory state) (instructionPointer state))

isExit intcode = case instructionFromIntcode intcode of
    99 -> True
    _ -> False
                                           
opcodeToOperation :: Int -> (ComputerState -> ComputerState)
opcodeToOperation intcode = case instructionFromIntcode intcode of
    1 -> \state -> -- add 
        let a = parameter intcode 1 state
            b = parameter intcode 2 state
            r = immediateParam 3 state
        in state {instructionPointer=(instructionPointer state) + 4, memory=Seq.update r (a+b) (memory state)}
    2 -> \state -> -- multiply
        let a = parameter intcode 1 state
            b = parameter intcode 2 state
            r = immediateParam 3 state
        in state {instructionPointer=(instructionPointer state) + 4, memory=Seq.update r (a*b) (memory state)}
    3 -> \state -> -- store
        let r = immediateParam 1 state
        in state {instructionPointer=(instructionPointer state) + 2, memory=Seq.update r (input state) (memory state)}
    4 -> \state -> -- read
        let a = parameter intcode 1 state
        in state {instructionPointer=(instructionPointer state) + 2, output=a}
    5 -> \state -> -- jump if a not zero
        let a = parameter intcode 1 state
            b = parameter intcode 2 state
        in state {instructionPointer=if a /= 0 then b else (instructionPointer state) + 3}
    6 -> \state -> -- jump if a zero
        let a = parameter intcode 1 state
            b = parameter intcode 2 state
        in state {instructionPointer=if a == 0 then b else (instructionPointer state) + 3}
    7 -> \state -> -- less than 
        let a = parameter intcode 1 state
            b = parameter intcode 2 state
            r = immediateParam 3 state
        in state {instructionPointer=(instructionPointer state) + 4, memory=Seq.update r (if a < b then 1 else 0) (memory state)}
    8 -> \state -> -- equal 
        let a = parameter intcode 1 state
            b = parameter intcode 2 state
            r = immediateParam 3 state
        in state {instructionPointer=(instructionPointer state) + 4, memory=Seq.update r (if a == b then 1 else 0) (memory state)}

instructionFromIntcode intcode = (read . reverse . take 2 . reverse . show) intcode

parameter intcode offset state
    | parameterMode intcode offset == Position = positionParam offset state
    | parameterMode intcode offset == Immediate = immediateParam offset state

parameterMode :: Int -> Int -> ParameterMode
parameterMode intcode offset 
    | offset + 2 > (length . show) intcode  = Position
    | otherwise = case ((drop 2 . reverse . show) intcode) !! (offset - 1) of
        '0' -> Position
        '1' -> Immediate 

data ParameterMode = Immediate | Position deriving (Eq)
 
positionParam offset state = readAddress (memory state) (readAddress (memory state) ((instructionPointer state) + offset))
immediateParam offset state = readAddress (memory state) ((instructionPointer state) + offset)

data ComputerState = ComputerState {
    input :: Int,
    instructionPointer :: Int,
    memory :: Seq.Seq Int,
    output :: Int
} deriving (Eq, Show)

 
readAddress memory index = Seq.index memory index

