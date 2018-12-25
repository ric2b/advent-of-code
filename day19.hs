import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.List (stripPrefix)

import Device

main = do
    input <- readFile "input/day19.txt"
    -- input <- readFile "input/day19.example.txt"
    let instructionPointer = read $ fromJust $ stripPrefix "#ip " $ head $ lines input
    let program = map parseInstruction $ drop 1 $ lines input
    let initialState = (Map.fromList (zip [0..5] (repeat 0)))
    part1
    print $ fromJust $ Map.lookup 0 $ runFlowProgram initialState instructionPointer program
    -- part2 - DANGEROUS, RAM EATER
    -- let initialState2 = Map.insert 0 1 initialState
    -- print $ fromJust $ Map.lookup 0 $ runFlowProgram initialState2 instructionPointer program


parseInstruction :: String -> Instruction
parseInstruction = toInstruction . words
    where toInstruction (opcode:a:b:resultRegister:[]) = Instruction opcode (read a) (read b) (read resultRegister)