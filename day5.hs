import Data.Char
import Data.List
import qualified Data.Set as Set

part1 :: String -> Int
part1 polymers = length $ foldr react "" polymers
    where
        react x (y:ys) | x /= y && toUpper x == toUpper y = ys
        react x ys = x : ys

part2 :: String -> Int
part2 polymers = minimum $ map (part1WithFilter polymers) (distinctPolymers polymers)
    where
        distinctPolymers polymers = nub $ map toLower polymers 
        part1WithFilter polymers removed = part1 $ filter ((removed /= ) . toLower) polymers

main :: IO() 
main = do
    input <- readFile "input5.txt"
    let polymers = head (lines input)
    print $ part1 polymers
    print $ part2 polymers