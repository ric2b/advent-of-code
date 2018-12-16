import Data.List
-- N marbles, numbered [1..N[
-- highest score after last marble
-- points: if marble is multiple of 23, keep marble (add to score), remove marble 7 cc and add to score

-- Note: compile with -O2

main :: IO() 
main = do
    input <- words <$> readFile "input/day9.txt"
    let nPlayers = read $ input !! 0 ::Int
    let maxMarble = read $ input !! 6 ::Int
    print $ part1 nPlayers maxMarble
    --print $ part2 nPlayers (maxMarble * 100)

placeIndex = 2
removeIndex = 7
buzzer = 23    

type Player = Int
type Score = Int
type Marble = Int

placeMarble :: Marble -> [Marble] -> [Marble]
placeMarble newMarble placedMarbles
    | placedMarbles == [] = newMarble:[0]  -- Starts with the 0 marble in place
    | otherwise = newMarble : drop placeIndex placedMarbles ++ take placeIndex placedMarbles   


takeMarble :: [Marble] -> (Marble, [Marble])
takeMarble placedMarbles = (reversedMarbles !! removeIndex, reverse (drop (removeIndex + 1) reversedMarbles ++ take removeIndex reversedMarbles))
    where reversedMarbles = reverse (tail placedMarbles ++ [head placedMarbles])


-- The Marble list should be sorted!    
makePlay :: Score -> [Marble] -> [Marble] -> (Score, [Marble], [Marble])
makePlay playerScore marbles placedMarbles
    | head marbles `mod` buzzer == 0 = (playerScore + (head marbles) + fst (takeMarble placedMarbles), tail marbles, snd (takeMarble placedMarbles))
    | otherwise = (playerScore, tail marbles, placeMarble (head marbles) placedMarbles)


playGame :: Int -> [Score] -> [Marble] -> [Marble] -> [Score]
playGame _ scores [] _ = scores
playGame currentPlayer scores nextMarbles placedMarbles = 
    playGame 
        ((currentPlayer + 1) `mod` length scores) 
        (changeScore currentPlayer (takeScore currentPlay) scores) 
        (takeNextMarbles currentPlay) 
        (takePlacedMarbles currentPlay)
        where 
            currentPlay = makePlay (scores !! currentPlayer) nextMarbles placedMarbles
            takeScore (score, _, _) = score
            takeNextMarbles (_, nextMarbles, _) = nextMarbles
            takePlacedMarbles (_, _, placedMarbles) = placedMarbles
            changeScore index newScore scores = take index scores ++ [newScore] ++ drop (index + 1) scores


part1 :: Int -> Int -> Int    
part1 players marbles = maximum $ playGame 0 (replicate players 0) [1..marbles] []