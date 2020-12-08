import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (concatMap, union, (\\))

import Text.ParserCombinators.Parsec

-- import Debug.Trace

main = do
    input <- lines <$> readFile "input/day08.txt"
    let instructions =  map ((\(Right result) -> result) . parse parseInstruction "") input
    
    print $ accumulator $ runUntilRepeats instructions S.empty initialMachineState -- 1832
    
    let comesFromGraph = buildComesFromGraph $ instructions ++ [("terminate", 0)]
    let terminatingIndexes = nodesReaching comesFromGraph [] (length instructions)
    let possibleFixes = filter (doesFixingTerminate terminatingIndexes) (zip [0..] instructions)
    let viableFixes = filter ((0 `elem`) . (nodesReaching comesFromGraph []) . fst) possibleFixes
    let fixedInstructions = fixInstruction instructions (fst $ head viableFixes)

    print $ accumulator $ runProgram (fixedInstructions) initialMachineState -- 662, after fixing 359

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

fixInstruction instructions index = replace instructions index (newOpCode, argument)
    where (opCode, argument) = instructions !! index
          newOpCode = case opCode of
              "jmp" -> "nop"
              "nop" -> "jmp"
              x -> x

doesFixingTerminate :: [Int] -> (Int, Instruction) -> Bool
doesFixingTerminate terminatingIndexes (index, (opCode, argument)) = 
    not (index `elem` terminatingIndexes) && case opCode of
        "jmp" -> index + 1 `elem` terminatingIndexes
        "nop" -> index + argument `elem` terminatingIndexes
        _ -> False

nodesReaching :: (Ord k) => M.Map k [k] -> [k] -> k -> [k]
nodesReaching inverseGraph alreadyVisited node = union newVisited indirectNeighbours
    where newDirectNeighbours = inverseGraph M.! node \\ alreadyVisited
          newVisited = union alreadyVisited newDirectNeighbours
          indirectNeighbours = concatMap (nodesReaching inverseGraph newVisited) newDirectNeighbours

buildComesFromGraph instructions = foldl addFroms graphWithDefaults instructionsWithIndexes
    where addFroms currentGraph (index, (opCode, argument)) = M.adjust (\comesFrom -> index:comesFrom) landsIn currentGraph
            where landsIn = if opCode == "jmp" then index + argument else index + 1
          graphWithDefaults = M.fromList [(i, []) | (i, _) <- instructionsWithIndexes]
          instructionsWithIndexes = zip [0..] instructions

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
