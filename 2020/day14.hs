{-# LANGUAGE NamedFieldPuns #-}

import Text.ParserCombinators.Parsec
import qualified Data.Map as M
import Data.Either (rights)

main = do
    rawInput <- lines <$> readFile "input/day14.txt"
    let instructions = rights $ map (parse parseInstruction "") rawInput
    let initialState = MachineState { memory = M.empty, mask = (take bitLength (repeat Nothing)) }

    print $ sum $ M.elems $ memory $ foldl runInstructionV1 initialState instructions -- 9615006043476
    print $ sum $ M.elems $ memory $ foldl runInstructionV2 initialState instructions -- 4275496544925

data MachineState = MachineState {
        memory :: M.Map Int Int,
        mask :: [Maybe Bool]
    } deriving (Show)

data Instruction = Mask [Maybe Bool] | Update (Int, Int) deriving (Show)

bitLength = 36

runInstructionV2 :: MachineState -> Instruction -> MachineState
runInstructionV2 currentState@(MachineState{memory, mask}) (Mask newMask) = currentState{mask = newMask}
runInstructionV2 currentState@(MachineState{memory, mask}) (Update (address, value)) = currentState{memory = newMemory}
    where newMemory = M.union updatedAddresses memory
          updatedAddresses = M.fromList $ map (\addr -> (toDecimal addr, value)) $ applyMaskAsFloating mask (toPaddedBinary bitLength address)

applyMaskAsFloating :: [Maybe Bool] -> [Int] -> [[Int]]
applyMaskAsFloating [] [] = [[]]
applyMaskAsFloating (m:ms) (a:as) = case m of
    Nothing -> concat [map (0:) restOfPossibilities, map (1:) restOfPossibilities]
    (Just True) -> map (1:) restOfPossibilities
    (Just False) -> map (a:) restOfPossibilities
    where restOfPossibilities = applyMaskAsFloating ms as

runInstructionV1 :: MachineState -> Instruction -> MachineState
runInstructionV1 currentState@(MachineState{memory, mask}) (Mask newMask) = currentState{mask = newMask}
runInstructionV1 currentState@(MachineState{memory, mask}) (Update (address, value)) = currentState{memory = newMemory}
    where newMemory = M.insert address (toDecimal $ applyMask mask (toPaddedBinary bitLength value)) memory

applyMask :: [Maybe Bool] -> [Int] -> [Int]
applyMask mask value = map applyMaskBit (zip mask value)
    where applyMaskBit (Nothing, inputBit) = inputBit
          applyMaskBit ((Just True), _) = 1
          applyMaskBit ((Just False), _) = 0

toPaddedBinary :: Int -> Int -> [Int]
toPaddedBinary size number = lpad size (toBinary number)
    where lpad m xs = replicate (m - length ys) 0 ++ ys
            where ys = take m xs

toBinary :: Int -> [Int]
toBinary 0 = [0]
toBinary n = toBinary (n `quot` 2) ++ [n `rem` 2]

toDecimal :: [Int] -> Int
toDecimal [] = 0
toDecimal (x:xs) = x * 2^(length xs) + (toDecimal xs)

-- parsing

readBit 'X' = Nothing
readBit '1' = Just True
readBit '0' = Just False

parseInstruction = (try parseMask) <|> parseUpdate

parseMask :: Parser Instruction
parseMask = do
    string "mask = "
    mask <- map readBit <$> many1 anyChar
    return (Mask mask)

parseUpdate :: Parser Instruction
parseUpdate = do
    string "mem["
    address <- read <$> many1 digit
    string "] = "
    value <- read <$> many1 digit
    return (Update (address, value))
