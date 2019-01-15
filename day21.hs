import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.List (stripPrefix, findIndex)

import Device

main = do
    input <- readFile "input/day21.txt"
    let instructionPointer = read $ fromJust $ stripPrefix "#ip " $ head $ lines input
    let program = map parseInstruction $ tail $ lines input
    let initialState = (Map.fromList (zip [0..5] (repeat 0)))
    -- part1
        -- the only operation that uses r0 is a gtrr with r1, which stops the program
        -- therefore all you need to do is reverse it and execute the program and find the big number at the end
    let modifiedExit = Instruction "seti" 1 0 3 
    let modifiedProgram = replaceElement program (findExitCondition program) modifiedExit
    print $ maximum $ runFlowProgram initialState instructionPointer modifiedProgram
    -- part2
    
replaceElement :: [a] -> Int -> a -> [a]
replaceElement list index element = start ++ [element] ++ end
        where start = take index list
              end = drop (index+1) list

-- eqrr instruction with r0 as argument    
findExitCondition :: [Instruction] -> Int
findExitCondition program = fromJust $ findIndex exitConditionPattern program    
        where exitConditionPattern i = (opcode i == "eqrr") && ((inputA i == 0) || (inputB i == 0))

parseInstruction :: String -> Instruction
parseInstruction = toInstruction . words
    where toInstruction (opcode:a:b:resultRegister:[]) = Instruction opcode (read a) (read b) (read resultRegister)