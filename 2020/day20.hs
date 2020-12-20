import Text.ParserCombinators.ReadP
import Data.Char (isDigit, isPrint, isAscii)
import Data.List (transpose, find)
import Data.Maybe (isJust, catMaybes)

import qualified Data.Map as M
import qualified Data.Set as S

main = do
    rawInput <- readFile "input/day20.txt"
    let tiles = M.fromList $ parse ((many1 tile) <* eof) rawInput
    let tileBorders = M.map borders tiles
    -- print tileEdgesIndex
    -- print $ length tiles
    -- print $ length $ filter (isEdgeTile tileBorders) (M.keys tiles)
    -- print $ length $ filter (isCornerTile tileBorders) (M.keys tiles)
    -- print $ length $ M.filter (==1) $ countEdges tileBorders

    print $ foldl1 (*) $ filter (isCornerTile tileBorders) (M.keys tiles) -- 15006909892229
    
    let Just startingCorner = find (isCornerTile tileBorders) (M.keys tiles)
    print startingCorner

    let allNeighbours = M.mapWithKey (\tileId _ -> matchAllEdges tileBorders tileId) tileBorders
    print $ allNeighbours M.! startingCorner

parse :: ReadP a -> String -> a
parse parser = fst . head . readP_to_S parser

data TileBorder = TileBorder {left::String, top::String, right::String, bottom::String} deriving Show
data Transform = RotateLeft Int | FlipHorizontal | FlipVertical | Diagonal1 | Diagonal2 deriving Show
data Direction = West | North | East | South deriving Show
data Neighbours = Neighbours {leftN::MaybeNeighbour, topN::MaybeNeighbour, rightN::MaybeNeighbour, bottomN::MaybeNeighbour} deriving Show
type MaybeNeighbour = Maybe (Int, Transform)

allTransforms :: TileBorder -> [(Transform, TileBorder)]
allTransforms border = zip possibleTransforms $ map ((flip transformBorder) border) possibleTransforms

possibleTransforms = [
        RotateLeft 0, RotateLeft 90, RotateLeft 180, RotateLeft 270, 
        FlipHorizontal, FlipVertical, Diagonal1, Diagonal2
    ]

transformBorder :: Transform -> TileBorder -> TileBorder
transformBorder transform border@(TileBorder{ left = l, top = t, right = r, bottom = b }) = case transform of
    (RotateLeft 0) -> border
    (RotateLeft 90) -> TileBorder{ left = reverse t, top = r, right = reverse b, bottom = l }
    (RotateLeft 180) -> transformBorder (RotateLeft 90) $ transformBorder (RotateLeft 90) border
    (RotateLeft 270) -> transformBorder (RotateLeft 180) $ transformBorder (RotateLeft 90) border
    FlipHorizontal -> TileBorder{ left = r, top = reverse t, right = l, bottom = reverse b }
    FlipVertical -> TileBorder{ left = reverse l, top = b, right = reverse r, bottom = t }
    Diagonal1 -> transformBorder (RotateLeft 90) $ transformBorder FlipVertical border
    Diagonal2 -> transformBorder (RotateLeft 90) $ transformBorder FlipHorizontal border

matchAllEdges :: M.Map Int TileBorder -> Int -> Neighbours
matchAllEdges tileBorders tileId = Neighbours {
        leftN = topNeighbourAfterRotating (RotateLeft 270), 
        topN = topNeighbourAfterRotating (RotateLeft 0), 
        rightN = topNeighbourAfterRotating (RotateLeft 90), 
        bottomN = topNeighbourAfterRotating (RotateLeft 180)
    }
    where topNeighbourAfterRotating rotation = M.lookupGT (-1) $ M.mapMaybe (matchTransformTop tileBorder rotation) withoutTile 
          withoutTile = M.delete tileId tileBorders
          tileBorder = tileBorders M.! tileId

matchTransformTop :: TileBorder -> Transform -> TileBorder -> Maybe Transform
matchTransformTop currentBorder currentTransform candidateBorder = case matchingTransform of
    Nothing -> Nothing
    Just (transform, _) -> Just transform
    where matchingTransform = find ((== currentBorderTop) . bottom . snd) (allTransforms candidateBorder)
          currentBorderTop = top $ transformBorder currentTransform currentBorder

countEdges :: M.Map Int TileBorder -> M.Map String Int
countEdges tileBorders = foldl edgeCounter M.empty $ concat $ map borderToEdgeList $ M.elems tileBorders
    where edgeCounter edgeCount edge =
            if edge == (reverse edge) || (reverse edge) `M.member` edgeCount
            then M.insertWith (+) (reverse edge) 1 edgeCount
            else M.insertWith (+) edge 1 edgeCount

            --foldlWithKey :: (a -> k -> b -> a) -> a -> Map k b -> a 
-- edgeReverseIndex :: M.Map Int TileBorder -> M.Map String [Int]
-- edgeReverseIndex tileBorders = foldlWithKey edgeIndexer M.empty tileBorders
--     where edgeIndexer edgeCount tileId border = foldl <> (borderToEdgeList border)
--             if edge == (reverse edge) || (reverse edge) `M.member` edgeCount
--             then M.insertWith (++) (reverse edge) tileId edgeCount
--             else M.insertWith (++) edge tileId edgeCount
            


isCornerTile tileBorders tileId = (==2) $ length $ uniqueEdges tileBorders tileId
isEdgeTile tileBorders tileId = (<4) $ length $ uniqueEdges tileBorders tileId

uniqueEdges :: M.Map Int TileBorder -> Int -> [String]
uniqueEdges tileBorders tileId = filter (`S.member` otherEdges) tileEdgesWithFlips
    where tileEdgesWithFlips = tileEdges ++ map reverse tileEdges
          tileEdges = borderToEdgeList $ tileBorders M.! tileId
          otherEdges = S.fromList $ concat $ map borderToEdgeList $ M.elems $ M.delete tileId tileBorders          

borderToEdgeList :: TileBorder -> [String]
borderToEdgeList TileBorder{left=l, top=t, right=r, bottom=b} = [l, t, r, b]

borders :: [String] -> TileBorder
borders rows = TileBorder {
    left = head (transpose rows),
    top = head rows,
    right = last (transpose rows),
    bottom = last rows
}

tile :: ReadP (Int, [String])
tile = do
    string "Tile"
    skipSpaces
    id <- read <$> many1 (satisfy isDigit)
    string ":\n"
    rows <- sepBy1 (many1 (char '.' +++ char '#')) (char '\n')
    string "\n\n"
    return (id, rows)
