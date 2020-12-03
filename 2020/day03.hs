main = do
    input <- lines <$> readFile "input/day03.txt"
    let baseMap = map loadLine input
    print $ head $ treesOnSlopes baseMap [(3, 1)] -- 274
    print $ foldl1 (*) $ treesOnSlopes baseMap [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)] -- 6050183040

type BaseMap = [[Bool]]

treesOnSlopes :: BaseMap -> [(Int, Int)] -> [Int]
treesOnSlopes baseMap slopes = map (treesEncontered baseMap) slopes
    where treesEncontered baseMap (speed_x, speed_y) = length $ filter (treeAt baseMap) $ traverseToBottom baseMap (0, 0) (speed_x, speed_y)

traverseToBottom :: BaseMap -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
traverseToBottom baseMap (x, y) (speed_x, speed_y) | y < baseMapHeight = [(x, y)] ++ traverseToBottom baseMap (x + speed_x `mod` baseMapWidth, y + speed_y) (speed_x, speed_y)
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
