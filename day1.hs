import qualified Data.IntSet as Set

removePlus :: Char -> Char
removePlus '+' = ' '
removePlus c = c

cleanIntString :: String -> String
cleanIntString intString = map removePlus intString 

cleanIntStrings :: [String] -> [String]
cleanIntStrings intStrings = map cleanIntString intStrings

stringsToInts :: [String] -> [Int]
stringsToInts intStrings = map read (cleanIntStrings intStrings)

part1 :: [Int] -> Int
part1 ints = sum ints

part2 :: [Int] -> Int
part2 ints = loop (cycle ints) 0 Set.empty
    where 
        loop (current:ints) total seen 
            | total `Set.member` seen = total
            | otherwise = loop ints (total+current) (total `Set.insert` seen)

main :: IO ()
main = do
    input <- readFile "input1.txt"
    let ints = stringsToInts (lines input)
    print (part1 ints)
    print (part2 ints)
