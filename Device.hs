module Device (
    runProgram,
    runFlowProgram,
    Instruction(..)
) where

import qualified Data.Map as Map
import Data.Bits
import Data.Maybe (fromJust)

import Debug.Trace

type RegisterId = Int
type State = Map.Map RegisterId Int
type Opcode = String
type Operation = State -> (Int, Int) -> Int

data Instruction = Instruction {
    opcode :: Opcode,
    inputA :: Int,
    inputB :: Int,
    resultRegister :: RegisterId
} deriving (Show)

runProgram :: Int -> [Instruction] -> State
runProgram nRegisters instructions = foldl runInstruction (Map.fromList (zip [0..nRegisters-1] (repeat 0))) instructions

runFlowProgram :: State -> Int -> [Instruction] -> State
runFlowProgram state instructionPointer instructions
    -- | trace ("ip=" ++ show currentIP ++ " " ++ show (Map.elems state)) False = undefined
    | succ currentIP < 0 || succ currentIP >= length instructions = state
    | otherwise = runFlowProgram newState instructionPointer instructions                
        where newState = Map.adjust succ instructionPointer $ runInstruction state currentInstruction
                where currentInstruction = instructions !! (fromJust $ Map.lookup instructionPointer state)
              currentIP = fromJust $ Map.lookup instructionPointer state                

runInstruction :: State -> Instruction -> State
runInstruction state instruction = Map.insert (resultRegister instruction) result state
        where result = operation state (inputA instruction, inputB instruction)
                where operation = opcodes Map.! (opcode instruction)

opcodes :: Map.Map Opcode Operation
opcodes = Map.fromList[
        ("addr", \s (a, b) -> (s Map.! a) + (s Map.! b)),
        ("addi", \s (a, b) -> (s Map.! a) + b),
        ("mulr", \s (a, b) -> (s Map.! a) * (s Map.! b)),
        ("muli", \s (a, b) -> (s Map.! a) * b),
        ("banr", \s (a, b) -> (s Map.! a) .&. (s Map.! b)),
        ("bani", \s (a, b) -> (s Map.! a) .&. b),
        ("borr", \s (a, b) -> (s Map.! a) .|. (s Map.! b)),
        ("bori", \s (a, b) -> (s Map.! a) .|. b),
        ("gtir", \s (a, b) -> if a > (s Map.! b) then 1 else 0),
        ("gtri", \s (a, b) -> if (s Map.! a) > b then 1 else 0),
        ("gtrr", \s (a, b) -> if (s Map.! a) > (s Map.! b) then 1 else 0),
        ("eqir", \s (a, b) -> if a == (s Map.! b) then 1 else 0),
        ("eqri", \s (a, b) -> if (s Map.! a) == b then 1 else 0),
        ("eqrr", \s (a, b) -> if (s Map.! a) == (s Map.! b) then 1 else 0),
        ("setr", \s (a, b) -> (s Map.! a)),
        ("seti", \s (a, b) -> a)
    ]