import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.List (stripPrefix)

import Device

main = do
    input <- readFile "input/day19.txt"
    let instructionPointer = read $ fromJust $ stripPrefix "#ip " $ head $ lines input
    let program = map parseInstruction $ drop 1 $ lines input
    let initialState = (Map.fromList (zip [0..5] (repeat 0)))
    -- part1
    print $ fromJust $ Map.lookup 0 $ runFlowProgram initialState instructionPointer program
    -- part2
    let myNumber = 10551381
    print $ sum [x | x <- [1,3..myNumber], myNumber `mod` x == 0]


parseInstruction :: String -> Instruction
parseInstruction = toInstruction . words
    where toInstruction (opcode:a:b:resultRegister:[]) = Instruction opcode (read a) (read b) (read resultRegister)