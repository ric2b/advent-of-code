import Text.ParserCombinators.ReadP
import Data.Char (isDigit, isPrint, isAscii)
import Data.List (transpose)

import qualified Data.Map as M
import qualified Data.Set as S

main = do
    rawInput <- readFile "input/day20.txt"
    let tiles = M.fromList $ parse ((many1 tile) <* eof) rawInput
    -- print $ take 5 $ tiles
    -- print $ (tileEdges . snd . head) tiles
    let tileEdgesIndex = M.map tileEdges tiles
    -- print tileEdgesIndex
    print $ length tiles
    print $ length $ filter (isEdgeTile tileEdgesIndex) (M.keys tiles)
    print $ length $ filter (isCornerTile tileEdgesIndex) (M.keys tiles)
    print $ foldl1 (*) $ filter (isCornerTile tileEdgesIndex) (M.keys tiles) -- 15006909892229

parse :: ReadP a -> String -> a
parse parser = fst . head . readP_to_S parser

isCornerTile :: M.Map Int [String] -> Int -> Bool
isCornerTile edgesIndex tileId = (==2) $ length $ filter (`S.member` otherEdges) tileEdgesWithFlips
    where tileEdgesWithFlips = tileEdges ++ map reverse tileEdges
          tileEdges = edgesIndex M.! tileId
          otherEdges = S.fromList $ concat $ M.elems $ M.delete tileId edgesIndex

isEdgeTile :: M.Map Int [String] -> Int -> Bool
isEdgeTile edgesIndex tileId = (<4) $ length $ filter (`S.member` otherEdges) tileEdgesWithFlips
    where tileEdgesWithFlips = tileEdges ++ map reverse tileEdges
          tileEdges = edgesIndex M.! tileId
          otherEdges = S.fromList $ concat $ M.elems $ M.delete tileId edgesIndex

tileEdges :: [String] -> [String]
tileEdges rows = [
        head (transpose rows),
        head rows,
        last (transpose rows),
        last rows
    ]

tile :: ReadP (Int, [String])
tile = do
    string "Tile"
    skipSpaces
    id <- read <$> many1 (satisfy isDigit)
    string ":\n"
    rows <- sepBy1 (many1 (char '.' +++ char '#')) (char '\n')
    string "\n\n"
    return (id, rows)
