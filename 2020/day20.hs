import Text.ParserCombinators.ReadP
import Data.Char (isDigit)
import Data.List (transpose, find)
import Data.Maybe (isJust)

import qualified Data.Map as M
import qualified Data.Set as S

main = do
    rawInput <- readFile "input/day20.txt"
    let tiles = M.fromList $ parse ((many1 parseTile) <* eof) rawInput
    -- let tileBorders = M.map gridBorders tiles
    let tileNeighbours = M.mapWithKey (\tileId _ -> matchAllEdges tiles tileId) tiles

    let cornerNeighbours = M.filter ((==2) . neighbourCount) tileNeighbours

    print $ foldl1 (*) $ M.keys $ cornerNeighbours -- 15006909892229
    
    let Just (startingCorner, rotation) = M.lookupGT (-1) $ M.map rotateCornerToTopLeft $ cornerNeighbours
    
    print startingCorner
    print rotation

    mapM_ print $ transformGrid rotation (tiles M.! startingCorner)
    let rotatedNeighbours = M.map (rotateNeighbours rotation) tileNeighbours

    -- print $ matchToRight tileNeighbours (Just (startingCorner, rotation)) -- 1321 -> 2381 -> 1321 -> 2381
    -- print $ matchDown rotatedNeighbours (Just (startingCorner, (RotateLeft 0)))

    -- print $ matchDown tileNeighbours (Just (startingCorner, rotation))

    print $ tileNeighbours M.! 1049
    -- print $ rotateNeighbours (RotateLeft 180) $ tileNeighbours M.! 1049
    print "-----------"
    print $ rotateNeighbours (RotateLeft 180) $ tileNeighbours M.! 1049
    print $ rotateNeighbours (FlipVertical) $ rotateNeighbours (RotateLeft 180) $ tileNeighbours M.! 1229
    print $ rotateNeighbours (FlipVertical) $ rotateNeighbours (FlipVertical) $ rotateNeighbours (RotateLeft 180) $ tileNeighbours M.! 2273
    print $ rotateNeighbours (RotateLeft 90) $ rotateNeighbours (FlipVertical) $ rotateNeighbours (FlipVertical) $ rotateNeighbours (RotateLeft 180) $ tileNeighbours M.! 2381
    print $ rotateNeighbours (Diagonal1) $ rotateNeighbours (RotateLeft 90) $ rotateNeighbours (FlipVertical) $ rotateNeighbours (FlipVertical) $ rotateNeighbours (RotateLeft 180) $ tileNeighbours M.! 1321
    -- print $ rotateNeighbours (RotateLeft 270) $ rotatedNeighbours M.! 1787
    -- print $ rotateNeighbours (RotateLeft 270) $ rotatedNeighbours M.! 1051
    -- print $ rotateNeighbours (Diagonal2) $ rotatedNeighbours M.! 2767
    -- print $ rotateNeighbours (FlipHorizontal) $ rotatedNeighbours M.! 2683
    -- print $ rotateNeighbours (RotateLeft 90) $ rotatedNeighbours M.! 3559
    -- print $ rotateNeighbours (RotateLeft 270) $ rotatedNeighbours M.! 2111
    -- print $ rotateNeighbours (RotateLeft 0) $ rotatedNeighbours M.! 3229

    mapM_ print $ transformGrid (RotateLeft 180) $ tiles M.! 1049
    print "-----------"
    -- mapM_ print $ tiles M.! 1229
    -- mapM_ print $ transformGrid (RotateLeft 180) $ transformGrid (Diagonal1) $ tiles M.! 1229
    -- mapM_ print $ transformGrid (FlipVertical) $ tiles M.! 1229
    -- mapM_ print $ transformGrid (RotateLeft 180) $ transformGrid (FlipVertical) $ tiles M.! 1229
    mapM_ print $ tiles M.! 2273
    print "-----------"
    mapM_ print $ transformGrid (FlipVertical) $ tiles M.! 2273
    print "-----------"
    -- mapM_ print $ tiles M.! 2273
    -- mapM_ print $ transformGrid (RotateLeft 180) $ tiles M.! 2273
    -- mapM_ print $ tiles M.! 2381
    mapM_ print $ transformGrid (RotateLeft 90) $ transformGrid (FlipVertical) $ transformGrid (FlipVertical) $ transformGrid (RotateLeft 180) $ tiles M.! 2381
    -- mapM_ print $ transformGrid (RotateLeft 270) $ tiles M.! 2381
    print "-----------"
    print $ tileNeighbours M.! 2381
    print $ rotateNeighbours (RotateLeft 90) $ rotateNeighbours (FlipVertical) $ rotateNeighbours (FlipVertical) $ rotateNeighbours (RotateLeft 180) $ tileNeighbours M.! 2381
    -- mapM_ print $ tiles M.! 1321
    -- mapM_ print $ transformGrid (Diagonal1) $ tiles M.! 1321
    mapM_ print $ transformGrid (Diagonal1) $ transformGrid (RotateLeft 90) $ transformGrid (FlipVertical) $ transformGrid (FlipVertical) $ transformGrid (RotateLeft 180) $ tiles M.! 1321
    -- mapM_ print $ transformGrid FlipVertical $ tiles M.! 1321
    -- mapM_ print $ transformGrid (FlipVertical) $ tiles M.! 2089
    -- print "-----------"
    -- mapM_ print $ transformGrid FlipVertical $ tiles M.! 1049
    -- mapM_ print $ transformGrid (RotateLeft 90) $ tiles M.! 1049

    

    -- print $ gridBorders $ tiles M.! 1049


    -- print $ rotatedNeighbours M.! 2273
    -- print $ rotatedNeighbours M.! 2381

    -- Neighbours {leftN = Just (2273,Diagonal1), topN = Nothing, rightN = Just (1049,Diagonal2), bottomN = Just (2957,RotateLeft 180)}
    -- Neighbours {leftN = Just (1049,Diagonal2), topN = Just (2957,RotateLeft 180), rightN = Just (2273,Diagonal1), bottomN = Nothing}
    -- Neighbours {leftN = Just (2957,RotateLeft 180), topN = Just (1049,Diagonal2), rightN = Nothing, bottomN = Just (2273,Diagonal1)}
    -- Neighbours {leftN = Nothing, topN = Just (2273,Diagonal1), rightN = Just (2957,RotateLeft 180), bottomN = Just (1049,Diagonal2)}

    --                1049          1229         2273         2381         1321
    -- Tile 1049:   < R 180       + Flip V     + Flip V     + R 90       + Flip V???????
    -- ###.##....   #..##.....   ..#.#.##.#   ##.##..##.   ...###.#..   .##......#
    -- ####....#.   #...##..##   ###...#..#   #.........   .....#....   .........#
    -- ...#.#....   ......####   #.....#...   ..........   ...#.#..#.   #.#.#.####
    -- ....##...#   .....#..##   #..#....#.   .#..##..##   #..#..#.#.   #....#...#
    -- #...#....#   ..###...##   #.##..##.#   #.......##   #.#.#..#.#   ###.#..###
    -- ##...###..   #....#...#   ###...#..#   ##.#.#.#..   .......#.#   #.#..##.##
    -- ##..#.....   #...##....   ..........   .......###   ##...#.###   .#..##.#..
    -- ####......   ....#.#...   ..###.....   .#...##..#   #......###   ###....#..
    -- ##..##...#   .#....####   ##..#..#..   ..........   .#.#.#...#   #.......#.
    -- .....##..#   ....##.###   ##....#.##   ##....##.#   #.##.#..##   .###...##.

    -- Tile 1229:   < Flip V >   < FV > R180 >
    -- #.##.#.#..   ##.#....##   ..#.#.##.#
    -- #..#...###   ..#..#..##   ###...#..#
    -- ...#.....#   .....###..   #.....#...
    -- .#....#..#   ..........   #..#....#.
    -- #.##..##.#   #..#...###   #.##..##.#
    -- #..#...###   #.##..##.#   ###...#..#
    -- ..........   .#....#..#   ..........
    -- .....###..   ...#.....#   ..###.....
    -- ..#..#..##   #..#...###   ##..#..#..
    -- ##.#....##   #.##.#.#..   ##....#.##

    -- Tile 2273:   < Flip V >
    -- #.##....##   .##..##.##
    -- ..........   .........#
    -- #..##...#.   ..........
    -- ###.......   ##..##..#.
    -- ..#.#.#.##   ##.......#
    -- ##.......#   ..#.#.#.##
    -- ##..##..#.   ###.......
    -- ..........   #..##...#.
    -- .........#   ..........
    -- .##..##.##   #.##....##
    
    -- Tile 2381:   < trnsf >
    -- ....######   ...###.#..
    -- ..##..##.#   .....#....
    -- #...####..   ...#.#..#.
    -- ...#......   #..#..#.#.
    -- ###...#.##   #.#.#..#.#
    -- #...#.....   .......#.#
    -- #.##....##   ##...#.###
    -- ....#....#   #......###
    -- ......#.#.   .#.#.#...#
    -- ...##.##.#   #.##.#..##

    -- Tile 1321:     < D1 >    < D1.trnsf >
    -- .###...##.   .##.####..   .##......#
    -- #.......#.   #.##.#...#   .........#
    -- ###....#..   #.#.##.#.#   #.#.#.####
    -- .#..##.#..   #.........   #....#...#
    -- #.#..##.##   ...#.#.#..   ###.#..###
    -- ###.#..###   ...##.#...   #.#..##.##
    -- #....#...#   ....#..#..   .#..##.#..
    -- #.#.#.####   #.##.#.#..   ###....#..
    -- .........#   ##..##.#..   #.......#.
    -- .##......#   ....######   .###...##.

    print $ matchTransformTop (tiles M.! 2381) (RotateLeft 0) (tiles M.! 1321)
    print $ tileNeighbours M.! 2381
    print $ rotateNeighbours (RotateLeft 90) $ rotateNeighbours (FlipVertical) $ rotateNeighbours (FlipVertical) $ rotateNeighbours (RotateLeft 180) $ tileNeighbours M.! 2381
    print $ rotateNeighbours (RotateLeft 270) $ tileNeighbours M.! 2381
    print "1321"
    mapM_ print $ tiles M.! 1321
    print "1321 - D1 + R270"
    mapM_ print $ transformGrid (RotateLeft 270) $ transformGrid Diagonal1 $ tiles M.! 1321
    print "1321 - R270 + D1"
    mapM_ print $ transformGrid Diagonal1 $ transformGrid (RotateLeft 270) $ tiles M.! 1321

    print $ matchTileRight (transformGrid (RotateLeft 180) $ tiles M.! 1049) (tiles M.! 1229)

    -- print $ matchToRight tiles (Just (startingCorner, rotation))
    let leftEdge = matchToBottom tiles (Just (startingCorner, rotation))
    let assembledPuzzle = map (matchToRight tiles . Just) leftEdge

    mapM_ print assembledPuzzle
    print $ M.keys cornerNeighbours


parse :: ReadP a -> String -> a
parse parser = fst . head . readP_to_S parser

-- data TileBorder = TileBorder {left::String, top::String, right::String, bottom::String} deriving Show
data Transform = RotateLeft Int | FlipHorizontal | FlipVertical | Diagonal1 | Diagonal2 deriving Show
data Neighbours = Neighbours {left::MaybeNeighbour, top::MaybeNeighbour, right::MaybeNeighbour, bottom::MaybeNeighbour} deriving Show
type MaybeNeighbour = Maybe (Int, Transform)
type Tile = [String]

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

-- matchDown :: M.Map Int Neighbours -> Maybe (Int, Transform) -> [(Int, Transform)]
-- matchDown tileNeighbours Nothing = []
-- matchDown tileNeighbours (Just (currentTile, rotation)) = (currentTile, rotation):(matchDown tileNeighbours bNeighbour)
--     where bNeighbour = bottomN $ rotateNeighbours rotation $ tileNeighbours M.! currentTile

matchAllEdges :: M.Map Int Tile -> Int -> Neighbours
matchAllEdges tiles tileId = Neighbours {
        left = topNeighbourAfterRotating (RotateLeft 270), 
        top = topNeighbourAfterRotating (RotateLeft 0), 
        right = topNeighbourAfterRotating (RotateLeft 90), 
        bottom = topNeighbourAfterRotating (RotateLeft 180)
    }
    where topNeighbourAfterRotating rotation = M.lookupGT (-1) $ M.mapMaybe (matchTransformTop tile rotation) withoutTile 
          withoutTile = M.delete tileId tiles
          tile = tiles M.! tileId

matchTransformTop :: Tile -> Transform -> Tile -> Maybe Transform
matchTransformTop currentTile currentTransform candidateTile = case matchingTransform of
    Nothing -> Nothing
    Just (transform, _) -> Just transform
    where matchingTransform = find ((== currentTileTop) . last . (transformGrid currentTransform) . snd) (allTransforms candidateTile)
          currentTileTop = head $ transformGrid currentTransform currentTile

matchToRight :: M.Map Int Tile -> Maybe (Int, Transform) -> [(Int, Transform)]
matchToRight tiles Nothing = []
matchToRight tiles (Just (currentTileId, rotation)) = (currentTileId, rotation):(matchToRight withoutTile rightNeighbour)
    where rightNeighbour = M.lookupGT (-1) $ M.mapMaybe (matchTileRight rotatedCurrentTile) withoutTile
          rotatedCurrentTile =  transformGrid rotation $ tiles M.! currentTileId
          withoutTile = M.delete currentTileId tiles

matchToBottom :: M.Map Int Tile -> Maybe (Int, Transform) -> [(Int, Transform)]
matchToBottom tiles Nothing = []
matchToBottom tiles (Just (currentTileId, rotation)) = (currentTileId, rotation):(matchToBottom withoutTile bottomNeighbour)
    where bottomNeighbour = M.lookupGT (-1) $ M.mapMaybe (matchTileBottom rotatedCurrentTile) withoutTile
          rotatedCurrentTile =  transformGrid rotation $ tiles M.! currentTileId
          withoutTile = M.delete currentTileId tiles
          

matchTileRight :: Tile -> Tile -> Maybe Transform
matchTileRight currentTile candidateTile = case matchingTransform of
    Nothing -> Nothing
    Just (transform, _) -> Just transform
    where matchingTransform = find ((== currentTileRight) . tileLeftBorder . snd) (allTransforms candidateTile)
          tileLeftBorder tile = head (transpose tile)
          currentTileRight = last (transpose currentTile)

matchTileBottom :: Tile -> Tile -> Maybe Transform
matchTileBottom currentTile candidateTile = case matchingTransform of
    Nothing -> Nothing
    Just (transform, _) -> Just transform
    where matchingTransform = find ((== currentTileBottom) . head . snd) (allTransforms candidateTile)
          currentTileBottom = last currentTile    
          

allTransforms :: Tile -> [(Transform, Tile)]
allTransforms tile = zip possibleTransforms $ map ((flip transformGrid) tile) possibleTransforms

possibleTransforms = [
        RotateLeft 0, RotateLeft 90, RotateLeft 180, RotateLeft 270, 
        FlipHorizontal, FlipVertical, Diagonal1, Diagonal2
    ]

-- gridBorders grid = TileBorder {
--     left = last (transformGrid (RotateLeft 90) grid), 
--     top = head grid, 
--     right = head (transformGrid (RotateLeft 90) grid), 
--     bottom = last grid
-- }

transformGrid :: Transform -> Tile -> Tile
transformGrid (RotateLeft 0) grid = grid
transformGrid (RotateLeft 90) grid = transformGrid FlipVertical $ transformGrid Diagonal1 grid
transformGrid (RotateLeft 180) grid = transformGrid FlipHorizontal $ transformGrid FlipVertical grid
transformGrid (RotateLeft 270) grid = transformGrid FlipHorizontal $ transformGrid Diagonal1 grid
transformGrid FlipHorizontal grid = map reverse grid
transformGrid FlipVertical grid = reverse grid
transformGrid Diagonal1 grid = transpose grid
transformGrid Diagonal2 grid = transformGrid (RotateLeft 90) $ transformGrid FlipVertical grid

-- transformBorder :: Transform -> TileBorder -> TileBorder
-- transformBorder transform border@(TileBorder{ left = l, top = t, right = r, bottom = b }) = case transform of
--     (RotateLeft 0) -> border
--     (RotateLeft 90) -> TileBorder{ left = reverse t, top = r, right = reverse b, bottom = l }
--     (RotateLeft 180) -> transformBorder (RotateLeft 90) $ transformBorder (RotateLeft 90) border
--     (RotateLeft 270) -> transformBorder (RotateLeft 180) $ transformBorder (RotateLeft 90) border
--     FlipHorizontal -> TileBorder{ left = r, top = reverse t, right = l, bottom = reverse b }
--     FlipVertical -> TileBorder{ left = reverse l, top = b, right = reverse r, bottom = t }
--     Diagonal1 -> transformBorder FlipVertical $ transformBorder (RotateLeft 90) border
--     Diagonal2 -> transformBorder FlipHorizontal $ transformBorder (RotateLeft 90) border

-- borders :: [String] -> TileBorder
-- borders rows = TileBorder {
--     left = head (transpose rows),
--     top = head rows,
--     right = last (transpose rows),
--     bottom = last rows
-- }

parseTile :: ReadP (Int, Tile)
parseTile = do
    string "Tile"
    skipSpaces
    id <- read <$> many1 (satisfy isDigit)
    string ":\n"
    rows <- sepBy1 (many1 (char '.' +++ char '#')) (char '\n')
    string "\n\n"
    return (id, rows)
