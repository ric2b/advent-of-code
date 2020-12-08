import qualified Data.Map as M
import qualified Data.Set as S

import Data.List (find)

import Text.ParserCombinators.Parsec

import Debug.Trace

main = do
    input <- lines <$> readFile "input/day08.txt"
    -- input <- lines <$> readFile "input/day08.example.txt"
    let instructions =  map ((\(Right result) -> result) . parse parseInstruction "") input
    -- let instructions = map (parse parseInstruction "") input
    -- print instructions
    -- print $ initialMachineState
    -- print $ executeInstruction initialMachineState (fetchInstruction instructions initialMachineState)
    -- print $ executeInstruction initialMachineState (instructions !! 1)
    -- print $ runProgram instructions initialMachineState
    -- print $ findCorruptInstruction instructions

    -- let fixedInstructions = replace instructions 7 ("nop", -4)
    -- let fixedInstructions = replace instructions 518 ("nop", 42)
    -- print $ Seq.index (Seq.update 518 ("nop", 42)  instructions) 518
    -- print $ runProgram fixedInstructions initialMachineState
    print $ accumulator $ runUntilRepeats instructions S.empty initialMachineState -- 1832


type Instruction = (OpCode, Argument)
type OpCode = String
type Argument = Int

data MachineState = MachineState {
        instructionPointer ::Int,
        accumulator ::Int
    } deriving (Show)

initialMachineState = MachineState { instructionPointer = 0, accumulator = 0 }

operations = M.fromList [
        ("nop", \s _ -> s),
        ("jmp", \s a -> s { instructionPointer = (instructionPointer s) + a - 1 }),
        ("acc", \s a -> s { accumulator = (accumulator s) + a })
    ]

runUntilRepeats :: [Instruction] -> S.Set Int -> MachineState -> MachineState
-- runUntilRepeats instructions previouslyExecuted currentState | trace (show currentState ++ show (fetchInstruction instructions currentState)) False = undefined
runUntilRepeats instructions previouslyExecuted currentState = 
    let currentIP = instructionPointer currentState
        alreadyExecuted = S.member currentIP previouslyExecuted
    in case alreadyExecuted of
        True -> currentState
        False -> runUntilRepeats instructions (S.insert currentIP previouslyExecuted) nextState
            where nextState = executeInstruction currentState nextInstruction
                  nextInstruction = fetchInstruction instructions currentState

runProgram :: [Instruction] -> MachineState -> MachineState
-- runProgram instructions currentState | trace (show currentState) False = undefined
runProgram instructions currentState = case instructionPointer currentState == length instructions of
    True -> currentState
    False -> runProgram instructions nextState
            where nextState = executeInstruction currentState nextInstruction
                  nextInstruction = fetchInstruction instructions currentState

fetchInstruction :: [Instruction] -> MachineState -> Instruction
fetchInstruction instructions MachineState{ instructionPointer = ip } = instructions !! ip

executeInstruction :: MachineState -> Instruction -> MachineState
executeInstruction currentState (opCode, argument) = newState { instructionPointer = (instructionPointer newState) + 1 }
    where newState = operation currentState argument
          operation = operations M.! opCode

parseInstruction :: Parser Instruction
parseInstruction = do
    opCode <- many1 letter
    char ' '
    optional (char '+') -- +42 doesn't parse as an Int, like -42 does
    argument <- read <$> many1 anyChar
    return (opCode, argument)

replace :: [a] -> Int -> a -> [a]
replace xs index newElement = case splitAt index xs of
   (before, _:after) -> before ++ newElement:after
   _ -> xs
