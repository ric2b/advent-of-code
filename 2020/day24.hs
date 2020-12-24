import qualified Data.Set as S
import Text.ParserCombinators.ReadP

main = do
    rawInput <- lines <$> readFile "input/day24.txt"
    let tiles = map (parse (parseTiles <* eof)) rawInput
    let initialBlackTiles = flippedToBlack $ map (foldl1 addVector . map vector) tiles

    print $ length initialBlackTiles -- 263
    print $ S.size $ iterate step (S.fromList initialBlackTiles) !! 100 -- 3649

data Direction = E | SE | SW | W | NW | NE deriving Show
type Position = (Int, Int)

step :: S.Set Position -> S.Set Position
step blackTiles = S.filter tileBecomesBlack relevantPositions
    where tileBecomesBlack position
            | position `S.member` blackTiles = blackNeighbourCount `elem` [1, 2] 
            | otherwise = blackNeighbourCount == 2
                where blackNeighbourCount = S.size ((neighbours position) `S.intersection` blackTiles)
          relevantPositions = S.foldr S.union blackTiles (S.map neighbours blackTiles)
          neighbours position = S.fromList $ map (addVector position . vector) [E, SE, SW, W, NW, NE]        

flippedToBlack :: [Position] -> [Position]
flippedToBlack positionsToFlip = S.toList $ foldl toggle S.empty positionsToFlip
    where toggle alreadyFlipped positionToFlip
            | positionToFlip `S.member` alreadyFlipped = S.delete positionToFlip alreadyFlipped
            | otherwise = S.insert positionToFlip alreadyFlipped

addVector :: Position -> Position -> Position
addVector (x, y) (a, b) = (x+a, y+b)

vector :: Direction -> Position
vector NE = (0, 1)
vector SW = (0, -1)
vector E = (1, 0)
vector W = (-1, 0)
vector NW = (-1, 1)
vector SE = (1, -1)

parse :: ReadP a -> String -> a
parse parser = fst . head . readP_to_S parser

parseTiles :: ReadP [Direction]
parseTiles = many1 (parseE +++ parseSE +++ parseSW +++ parseW +++ parseNW +++ parseNE)

parseE = E <$ string "e"
parseSE = SE <$ string "se"
parseSW = SW <$ string "sw"
parseW = W <$ string "w"
parseNW = NW <$ string "nw"
parseNE = NE <$ string "ne"
