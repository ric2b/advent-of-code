import qualified Data.Map.Strict as Map
import Data.List

letterFrequency :: String -> Map.Map Char Int
letterFrequency input = Map.fromListWith (+) [(c, 1) | c <- input]

frequency2Exists :: Map.Map Char Int -> Bool
frequency2Exists frequencies = 2 `elem` (Map.elems frequencies)

frequency3Exists :: Map.Map Char Int -> Bool
frequency3Exists frequencies = 3 `elem` (Map.elems frequencies)

stringDistance :: String -> String -> Int
stringDistance word1 word2 = sum [1 | (x, y) <- zip word1 word2, x /= y]

matchingLetters :: String -> String -> String
matchingLetters [] rest2 = []
matchingLetters rest1 [] = []
matchingLetters word1@(firstLetter1:rest1) word2@(firstLetter2:rest2)
    | firstLetter1 == firstLetter2 = firstLetter1:(matchingLetters (firstLetter1:rest1) rest2)
    | otherwise = rest1   

part2 strs = [ls `intersect` rs | ls <- strs, rs <- strs, stringDistance ls rs == 1]                  

main :: IO ()
main = do
    input <- readFile "input/day2.txt"
    let frequencies = map letterFrequency (lines input)
    let freq2total = (sum [1 | x <- (map frequency2Exists frequencies), x == True])
    let freq3total = (sum [1 | x <- (map frequency3Exists frequencies), x == True])
    print (freq2total * freq3total)
    let part2result = part2 (lines input)
    print (matchingLetters (part2result!!0) (part2result!!1))
