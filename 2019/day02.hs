#!/usr/bin/env runhaskell
import Data.Text (split, pack, unpack)
import qualified Data.Sequence as Seq

main :: IO ()
main = do
    input <- readFile "input/day02.txt"
    print (part1 input)
    print (part2 input)

part1 input = executeProgram (load 12 02 (loadMemory input)) 0
part2 input = [100*noun + verb | noun <- [0..99], verb <- [0..99], 
    Seq.index (executeProgram (load noun verb (loadMemory input)) 0) 0 == 19690720]

load noun verb memory = Seq.update 2 verb (Seq.update 1 noun memory)

loadMemory input = Seq.fromList intList
    where intList = map (read . unpack) splitByComma
            where splitByComma = split (==',') (pack (filter (/= '\n') input))

executeProgram :: Seq.Seq Int -> Int -> Seq.Seq Int
executeProgram memory programCounter = case isExit (readAddress memory programCounter) of
    True -> memory
    False -> executeProgram updatedMemory (programCounter + 4)
                where updatedMemory = executeOperation memory programCounter

executeOperation :: Seq.Seq Int -> Int -> Seq.Seq Int
executeOperation memory programCounter = operation programCounter memory
    where operation = opcodeToOperation (readAddress memory programCounter)

isExit 99 = True
isExit x = False

opcodeToOperation :: Int -> (Int -> Seq.Seq Int  -> Seq.Seq Int )
opcodeToOperation intcode = case intcode of
    1 -> \programCounter memory -> 
        let a = paramA memory programCounter
            b = paramB memory programCounter
            r = resultIndex memory programCounter
        in Seq.update r (a+b) memory
    2 -> \programCounter memory -> 
        let a = paramA memory programCounter
            b = paramB memory programCounter
            r = resultIndex memory programCounter
        in Seq.update r (a*b) memory

paramA = parameter 1
paramB = parameter 2
resultIndex = readOffset 3

parameter number memory programCounter = readAddress memory (readAddress memory (programCounter + number))
parameterIndex index = readOffset index
 
readOffset offset memory current = readAddress memory (current + offset) 
readAddress memory index = Seq.index memory index

