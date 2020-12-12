main = do
    raw_input <- lines <$> readFile "input/day12.txt"
    let navigationInstructions = map parseInstruction raw_input
    let initialShipState = ShipState{position=(0, 0), angle=0, waypoint=(10, 1)}

    print $ distanceMoved $ foldl moveForward initialShipState navigationInstructions -- 1457
    print $ distanceMoved $ foldl moveToWaypoint initialShipState navigationInstructions -- 106860

type Position = (Int, Int)
type Angle = Int
data ShipState = ShipState { position :: Position, angle :: Angle, waypoint :: Position } deriving (Show)

data Action = N | S | E | W | L | R | F deriving (Show)
type NavigationInstruction = (Action, Int)

distanceMoved ShipState{position=(x, y)} = abs(x) + abs(y)

moveToWaypoint :: ShipState -> NavigationInstruction -> ShipState
moveToWaypoint shipState@(ShipState{position=(x, y), waypoint=(a, b)}) (action, value) = case action of
    N -> shipState{waypoint=(a, b+value)}
    S -> shipState{waypoint=(a, b-value)}
    E -> shipState{waypoint=(a+value, b)}
    W -> shipState{waypoint=(a-value, b)}
    L -> shipState{waypoint=rotateN ((3*value) `div` (90)) (a, b)}
    R -> shipState{waypoint=rotateN (value `div` 90) (a, b)}
    F -> shipState{position=(x+(value*a), y+(value*b))}

rotateN :: Int -> Position -> Position
rotateN n position = (iterate rotate position) !! n

rotate :: Position -> Position
rotate (a, b) = (b, -a)

moveForward :: ShipState -> NavigationInstruction -> ShipState
moveForward shipState@(ShipState{position=(x, y), angle=angle}) (action, value) = case action of
    N -> shipState{position=(x, y+value)}
    S -> shipState{position=(x, y-value)}
    E -> shipState{position=(x+value, y)}
    W -> shipState{position=(x-value, y)}
    L -> shipState{angle=(angle - value) `mod` 360}
    R -> shipState{angle=(angle + value) `mod` 360}
    F -> case angle of 
        0 -> shipState{position=(x+value, y)}
        90 -> shipState{position=(x, y-value)}
        180 -> shipState{position=(x-value, y)}
        270 -> shipState{position=(x, y+value)}

parseInstruction :: String -> NavigationInstruction
parseInstruction raw_instruction = case head raw_instruction of
    'N' -> (N, value)
    'S' -> (S, value)
    'E' -> (E, value)
    'W' -> (W, value)
    'L' -> (L, value)
    'R' -> (R, value)
    'F' -> (F, value)
    where value = read $ tail raw_instruction
