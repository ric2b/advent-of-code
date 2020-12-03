main = do
    input <- lines <$> readFile "input/day03.txt"
    let baseMap = map (map ('#' ==)) input
    print $ head $ treesOnSlopes baseMap [(3, 1)] -- 274
    print $ foldl1 (*) $ treesOnSlopes baseMap [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)] -- 6050183040

type BaseMap = [[Bool]]

treesOnSlopes :: BaseMap -> [(Int, Int)] -> [Int]
treesOnSlopes baseMap slopes = map treesEncontered slopes
    where treesEncontered (speed_x, speed_y) = length $ filter (treeAt baseMap) $ traverseToBottom baseMap (0, 0) (speed_x, speed_y)

traverseToBottom :: BaseMap -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
traverseToBottom baseMap (x, y) (speed_x, speed_y) 
    | y >= length baseMap = []
    | otherwise = [(x, y)] ++ traverseToBottom baseMap (x + speed_x `mod` baseMapWidth, y + speed_y) (speed_x, speed_y)
        where baseMapWidth = length (head baseMap)

treeAt :: BaseMap -> (Int, Int) -> Bool
treeAt baseMap (x, y) = (baseMap !! y) !! (x `mod` baseMapWidth)
    where baseMapWidth = length (head baseMap)
