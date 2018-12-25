import qualified Data.Map as Map
import Data.Bits
import Data.List
import Data.Maybe (fromJust)

main = do
    input <- readFile "input/day16.txt"
    let samples = parseSamples $ lines input
    print $ part1 samples

    let opcodes = fst $ computeOpcodes (Map.empty, possibleInstructions samples)
    let program = map (fixInstruction opcodes . parseInstruction) $ (reverse . takeWhile (/= "") . reverse) $ lines input
    print $ fromJust $ Map.lookup 0 $ runProgram 4 program

{- Part 1 -}
part1 :: [Sample] -> Int
part1 samples = (length . filter ((>= 3) . length . snd) . map possibleInstructionsForState) samples
  
possibleInstructions :: [Sample] -> Map.Map Int [Opcode]
possibleInstructions samples = Map.fromListWith intersect $ map possibleInstructionsForState $ samples

possibleInstructionsForState :: Sample -> (Int, [Opcode])
possibleInstructionsForState (before, (rawOpcode, inputA, inputB, resultRegister), after) = 
    (rawOpcode, filter ((after ==) . apply) (Map.keys opcodes))
        where apply opcode = runInstruction before (Instruction opcode inputA inputB resultRegister)

{- Part 2 -}                
fixInstruction :: Map.Map Int Opcode -> RawInstruction -> Instruction
fixInstruction opcodes (opcode, a, b, resultRegister) = Instruction (opcodes Map.! opcode) a b resultRegister

computeOpcodes :: (Map.Map Int Opcode, Map.Map Int [Opcode]) -> (Map.Map Int Opcode, Map.Map Int [Opcode])
computeOpcodes (identifiedInstructions, possibleInstructions)
    | Map.null possibleInstructions = (identifiedInstructions, Map.empty)
    | otherwise = computeOpcodes (newIdentifiedInstructions, newPossibleInstructions)
        where   newIdentifiedInstructions = Map.union identifiedInstructions $ Map.map head $ Map.filter ((==1) . length) $ Map.map (filter (`notElem` (Map.elems identifiedInstructions))) possibleInstructions
                newPossibleInstructions = Map.filter ((>1) . length) $ Map.map (filter (`notElem` (Map.elems identifiedInstructions))) possibleInstructions

{- Interpreter -}
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

{- Input parsing -}
type RawInstruction = (Int, Int, Int, Int)
type Sample = (State, RawInstruction, State)

parseSamples :: [String] -> [Sample]
parseSamples lines = map parseSample $ filter (isPrefixOf "Before: " . head) $ chunks 4 lines
    where chunks n = takeWhile (not.null) . unfoldr (Just . splitAt n)

parseSample :: [String] -> Sample
parseSample lines = (before, rawInstruction, after)
    where   before = (parseState "Before: ") $ lines !! 0
            rawInstruction = parseInstruction $ lines !! 1
            after = (parseState "After: ") $ lines !! 2 

parseState :: String -> String -> Map.Map Int Int
parseState prefix = Map.fromList . (zip [0..]) . map read . words . filter (`notElem` "[],") . fromJust . stripPrefix prefix

parseInstruction :: String -> RawInstruction
parseInstruction = toRawInstruction . map read . words
    where toRawInstruction (opcode:inputA:inputB:resultRegister:[]) = (opcode, inputA, inputB, resultRegister)
