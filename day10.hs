import qualified Data.Set as Set

main :: IO() 
main = do
    input <- readFile "input/day10.txt"
    let messageTime = findMessage 0 $ parsePoint <$> lines input
    mapM_ putStrLn $ drawPoints $ movePoints messageTime $ parsePoint <$> lines input
    print messageTime

data Point = Point {
    xPosition :: Int,
    yPosition :: Int,
    xVelocity :: Int,
    yVelocity :: Int
} deriving (Show)

-- "position=< 9,  1> velocity=< 0,  2>" 
parsePoint :: String -> Point     
parsePoint line = makePoint $ map read $ words $ filter (`elem` "-0123456789 ") line
    where makePoint (xP:yP:xV:yV:_) = Point {xPosition=xP, yPosition=yP, xVelocity=xV, yVelocity=yV}

movePoints :: Int -> [Point] -> [Point]
movePoints times points = map updatePosition points
    where updatePosition point = Point {xPosition=newX, yPosition=newY, xVelocity=xVelocity point, yVelocity=yVelocity point}
            where 
                newX = xPosition point + times * xVelocity point
                newY = yPosition point + times * yVelocity point    

pointsArea :: [Point] -> Int
pointsArea points = abs (maxX - minX) * abs (maxY - minY)
    where         
        minX = minimum $ map xPosition points
        maxX = maximum $ map xPosition points
        minY = minimum $ map yPosition points
        maxY = maximum $ map yPosition points

-- Assuming minimum area means message is ready
findMessage :: Int -> [Point] -> Int
findMessage iteration points
    | pointsArea (movePoints iteration points) < pointsArea (movePoints (iteration+1) points) = iteration
    | otherwise = findMessage (iteration+1) points

drawPoints :: [Point] -> [String]
drawPoints points = [[if (x, y) `Set.member` pointSet then '#' else ' ' | x <- [minX..maxX]] | y <- [minY..maxY]]
    where 
        pointSet = Set.fromList $ map (\p -> (xPosition p, yPosition p)) points
        minX = minimum $ map xPosition points
        maxX = maximum $ map xPosition points
        minY = minimum $ map yPosition points
        maxY = maximum $ map yPosition points
