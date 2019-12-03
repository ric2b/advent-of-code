import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set


main :: IO() 
main = do
    input <- readFile "input/day12.txt"
    let initialState = parseState 0 $ fromJust $ stripPrefix "initial state: " $ head $ lines input
    let patterns = map parsePattern $ tail $ tail $ lines input
    print $ sum $ Set.toList $ getGeneration initialState patterns 20
    -- assuming it starts looping before 200:
    let loopBase = sum $ Set.toList $ getGeneration initialState patterns 200
    let loopInc = (sum $ Set.toList $ getGeneration initialState patterns 201) - loopBase
    print $ loopBase + (50000000000-200)*loopInc

flower = '#'
empty = '.'

type Pattern = (String -> (Bool, Char)) -- a function that matches the pattern to a string
type Pots = Set Int -- locations with flowers

parseState :: Int -> String -> Pots
parseState from input = Set.fromList $ map fst $ filter ((== flower) . snd) $ zip [from..] input

parsePattern :: String -> Pattern
parsePattern line = appendOutput . (==) pattern
    where pattern = takeWhile (/= ' ') line 
          appendOutput matched = (matched, last line)

viewPot :: Int -> Pots -> [Char]
viewPot index pots = [hasPot i | i <- [index-2..index+2]]
    where hasPot i = if i `Set.member` pots then flower else empty
          
agePot :: Pots -> [Pattern] -> Int -> Char
agePot pots patterns index = snd $ fromJust $ find fst $ map (\p -> p (viewPot index pots)) patterns

newGeneration :: Pots -> [Pattern] -> Pots
newGeneration pots patterns = parseState (minFlower-5) $ map (agePot pots patterns) [minFlower-5..maxFlower+5]
    where minFlower = Set.findMin pots
          maxFlower = Set.findMax pots

getGeneration :: Pots -> [Pattern] -> Int -> Pots
getGeneration pots patterns generationsLeft
    | generationsLeft <= 0 = pots
    | otherwise = getGeneration (newGeneration pots patterns) patterns (generationsLeft-1)
  
showPots :: Pots -> String
showPots pots = [if i `Set.member` pots then flower else empty | i <- [-3..maxFlower]]
    where minFlower = Set.findMin pots
          maxFlower = Set.findMax pots