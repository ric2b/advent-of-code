import Text.ParserCombinators.Parsec

main = do
    input <- lines <$> readFile "input/day03.txt"
    let baseMap = map loadLine input
    print $ length $ filter (treeAt baseMap) $ traverseToBottom baseMap (3, 1) (0, 0) -- 274

type BaseMap = [[Bool]]

traverseToBottom :: BaseMap -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
traverseToBottom baseMap (speed_x, speed_y) (x, y) | y < baseMapHeight = [(x, y)] ++ traverseToBottom baseMap (speed_x, speed_y) (x + speed_x `mod` baseMapWidth, y + speed_y)
                        | otherwise = []
                            where baseMapHeight = length baseMap
                                  baseMapWidth = (length . head) baseMap

loadLine :: String -> [Bool]
loadLine line = map isTree line
    where isTree '.' = False
          isTree '#' = True

treeAt :: BaseMap -> (Int, Int) -> Bool
treeAt baseMap (x, y) = (baseMap !! y) !! (x `mod` baseMapWidth)
    where baseMapWidth = (length . head) baseMap