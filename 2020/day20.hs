import Text.ParserCombinators.ReadP
import Data.Char (isDigit)
import Data.List (transpose, find, elemIndices, (\\))
import Data.Maybe (isJust)
import qualified Data.Map as M

main = do
    rawInput <- readFile "input/day20.txt"
    let tiles = M.fromList $ parse ((many1 parseTile) <* eof) rawInput
    let tileNeighbours = M.mapWithKey (\tileId _ -> matchAllEdges tiles tileId) tiles
    
    let cornerNeighbours = M.filter ((==2) . neighbourCount) tileNeighbours
    print $ foldl1 (*) $ M.keys $ cornerNeighbours -- 15006909892229

    let Just (startingCorner, rotation) = M.lookupGT (-1) $ M.map rotateCornerToTopLeft $ cornerNeighbours
    let leftEdge = matchToBottom tiles (Just (startingCorner, rotation))
    let tilePlacements = map (matchToRight tiles . Just) leftEdge
    let assembledImage = assembleImage tiles tilePlacements
    let monsterCount = sum $ map (findMonsters . snd) (allTransforms assembledImage)

    print $ (hashtagCount assembledImage) - monsterCount * (hashtagCount monster) -- 2190

parse :: ReadP a -> String -> a
parse parser = fst . head . readP_to_S parser

data Transform = RotateLeft Int | FlipHorizontal | FlipVertical | Diagonal1 | Diagonal2 deriving Show
data Neighbours = Neighbours {left::MaybeNeighbour, top::MaybeNeighbour, right::MaybeNeighbour, bottom::MaybeNeighbour} deriving Show
type MaybeNeighbour = Maybe (Int, Transform)
type Tile = [String]

hashtagCount image = length $ concat $ map (filter (=='#')) image

findMonsters :: [String] -> Int
findMonsters image = length $ filter includesMonster $ map (imageSlice (monsterWidth, monsterHeight) image) points
    where imageSlice (width, height) image (x,y) = map (slice x (x+width)) $ slice y (y+height) image
          slice begin end = drop begin . take end
          monsterWidth = length $ head monster
          monsterHeight = length monster
          points = [(x, y) | x <- [0..imageWidth], y <- [0..imageHeight], x < imageWidth, y < imageHeight]
          imageWidth = length $ head image
          imageHeight = length image

includesMonster image = (length image) == (length monster) && all null missingIndices
    where missingIndices = map (\(m, t) -> m \\ t) $ zip monsterIndices imageIndices
          monsterIndices = map (elemIndices '#') monster
          imageIndices = map (elemIndices '#') image
      
monster = [
        "                  # ",
        "#    ##    ##    ###",
        " #  #  #  #  #  #   "
    ]

assembleImage :: M.Map Int Tile -> [[(Int, Transform)]] -> [String]
assembleImage tiles tilePlacements = concat $ map (map concat . transpose . map asTile) tilePlacements
    where asTile (tileId, transform) = stripBorders $ transformGrid transform (tiles M.! tileId)
          stripBorders tile = (init . tail) $ map (init . tail) tile

rotateCornerToTopLeft Neighbours {left = l, top = t, right = r, bottom = b} = case (l, t, r, b) of
    (Nothing, Nothing, _, _) -> RotateLeft 0
    (_, Nothing, Nothing, _) -> RotateLeft 90
    (_, _, Nothing, Nothing) -> RotateLeft 180
    (Nothing, _, _, Nothing) -> RotateLeft 270

rotateNeighbours :: Transform -> Neighbours -> Neighbours
rotateNeighbours rotation neighbours@(Neighbours{left = l, top = t, right = r, bottom = b}) = case rotation of
    RotateLeft 0 -> neighbours
    RotateLeft 90 -> Neighbours{left = t, top = r, right = b, bottom = l}
    RotateLeft 180 -> rotateNeighbours (RotateLeft 90) $ rotateNeighbours (RotateLeft 90) neighbours
    RotateLeft 270 -> rotateNeighbours (RotateLeft 180) $ rotateNeighbours (RotateLeft 90) neighbours
    FlipHorizontal -> Neighbours{ left = r, top = t, right = l, bottom = b }
    FlipVertical -> Neighbours{ left = l, top = b, right = r, bottom = t }
    Diagonal1 -> rotateNeighbours FlipVertical $ rotateNeighbours (RotateLeft 90) neighbours
    Diagonal2 -> rotateNeighbours FlipHorizontal $ rotateNeighbours (RotateLeft 90) neighbours

neighbourCount Neighbours {left = l, top = t, right = r, bottom = b} = length $ filter isJust [l, t, r, b]

matchAllEdges :: M.Map Int Tile -> Int -> Neighbours
matchAllEdges tiles tileId = Neighbours {
        left = rightNeighbourAfterRotating (RotateLeft 180), 
        top = rightNeighbourAfterRotating (RotateLeft 270), 
        right = rightNeighbourAfterRotating (RotateLeft 0), 
        bottom = rightNeighbourAfterRotating (RotateLeft 90)
    }
    where rightNeighbourAfterRotating rotation = M.lookupGT (-1) $ M.mapMaybe (matchTileRight (transformGrid rotation tile)) withoutTile 
          withoutTile = M.delete tileId tiles
          tile = tiles M.! tileId

matchToRight, matchToBottom :: M.Map Int Tile -> Maybe (Int, Transform) -> [(Int, Transform)]
matchToRight = matchTo matchTileRight
matchToBottom = matchTo matchTileBottom

matchTo :: (Tile -> Tile -> Maybe Transform) -> M.Map Int Tile -> Maybe (Int, Transform) -> [(Int, Transform)]
matchTo _ _ Nothing = []
matchTo matchFunction tiles (Just (currentTileId, rotation)) = (currentTileId, rotation):(matchTo matchFunction withoutTile neighbour)
    where neighbour = M.lookupGT (-1) $ M.mapMaybe (matchFunction rotatedCurrentTile) withoutTile
          rotatedCurrentTile =  transformGrid rotation $ tiles M.! currentTileId
          withoutTile = M.delete currentTileId tiles

matchTileRight, matchTileBottom :: Tile -> Tile -> Maybe Transform
matchTileRight = matchTile matchRight
    where matchRight currentTile = (== (last (transpose currentTile))) . (head . transpose)
matchTileBottom = matchTile matchBottom
    where matchBottom currentTile = (== (last currentTile)) . head

matchTile :: (Tile -> Tile -> Bool) -> Tile -> Tile -> Maybe Transform
matchTile matchFunction currentTile candidateTile = case matchingTransform of
    Nothing -> Nothing
    Just (transform, _) -> Just transform
    where matchingTransform = find (matchFunction currentTile . snd) (allTransforms candidateTile)

allTransforms :: Tile -> [(Transform, Tile)]
allTransforms tile = zip possibleTransforms $ map ((flip transformGrid) tile) possibleTransforms

possibleTransforms = [RotateLeft 0, RotateLeft 90, RotateLeft 180, RotateLeft 270, FlipHorizontal, FlipVertical, Diagonal1, Diagonal2]

transformGrid :: Transform -> Tile -> Tile
transformGrid (RotateLeft 0) grid = grid
transformGrid (RotateLeft 90) grid = transformGrid FlipVertical $ transformGrid Diagonal1 grid
transformGrid (RotateLeft 180) grid = transformGrid FlipHorizontal $ transformGrid FlipVertical grid
transformGrid (RotateLeft 270) grid = transformGrid FlipHorizontal $ transformGrid Diagonal1 grid
transformGrid FlipHorizontal grid = map reverse grid
transformGrid FlipVertical grid = reverse grid
transformGrid Diagonal1 grid = transpose grid
transformGrid Diagonal2 grid = transformGrid (RotateLeft 90) $ transformGrid FlipVertical grid

parseTile :: ReadP (Int, Tile)
parseTile = do
    string "Tile"
    skipSpaces
    id <- read <$> many1 (satisfy isDigit)
    string ":\n"
    rows <- sepBy1 (many1 (char '.' +++ char '#')) (char '\n')
    string "\n\n"
    return (id, rows)
