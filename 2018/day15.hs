import qualified Data.Sequence as Seq
import Data.Foldable (toList)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.List (sortOn)

import qualified Graph as Graph
import Graph (Graph)

main :: IO() 
main = do
    input <- readFile "input/day15.txt"
    let dungeonState = parseState $ lines input 
    print $ length $ ground dungeonState
    let graph = makeGraph dungeonState
    print $ Graph.reachable (1,1) graph
    -- print $ map ((Map.!) (units dungeonState)) $ concatMap (Set.toList . (Graph.!) graph) $ Graph.reachable (10,7) graph
    print $ reachableEnemyGround dungeonState 10
    print $ closestEnemies dungeonState 10
    -- print $ position $ (units dungeonState) Map.! 10
    -- print $ Graph.shortestPaths (10, 7) (9, 6) $ graph

type Id = Int
type Position = (Int, Int)

data DungeonState = DungeonState {
    ground :: Set Position,
    units :: Map Id Unit
} deriving (Show)

data Unit = Unit {
    unitType :: UnitType,
    position :: Position,
    health :: Int
} deriving (Show)

data UnitType = Goblin | Elve deriving (Eq, Show)
data Structure = Ground | Wall

fullRound :: DungeonState -> DungeonState
fullRound dungeonState = dungeonState{units = foldl unitRound (units dungeonState) unitOrder}
    where unitOrder = sortOn (position . snd) (Map.assocs (units dungeonState)) 
          unitRound dungeonState unit = dungeonState

doMove :: DungeonState -> Id -> Position -> DungeonState
doMove dungeonState id newPosition = dungeonState{units = Map.adjust (changePosition) id (units dungeonState)}
    where changePosition unit = unit{position = newPosition}

doAttack :: DungeonState -> Id -> DungeonState
doAttack dungeonState id = dungeonState{units = Map.update damage id (units dungeonState)}
    where damage unit = if health unit > 3 then Just (unit{health = (health unit) - 3}) else Nothing

-- closestEnemies :: DungeonState -> Id -> [(Position, Int)]
closestEnemies dungeonState unitId = --map fst
    -- map (\(target, paths) -> (target, (length . head) paths))
    -- $ filter (not . null . snd)
    -- $ [(target, Graph.shortestPaths source target dungeonGraph) | source <- unitGround, target <- enemyGround]
    Graph.shortestPaths (11,19) (9,8) dungeonGraph
    where   dungeonGraph = makeGraph dungeonState
            unitGround = Graph.keys $ Graph.filter (unitId `Set.member`) dungeonGraph
            enemyGround = Set.toList $ reachableEnemyGround dungeonState unitId

reachableEnemyGround :: DungeonState -> Id -> Set Position
reachableEnemyGround dungeonState unitId  = Set.unions 
    $ map (flip (Graph.reachableWith isEnemyGround) dungeonGraph) unitGround
    where   friendlyType = unitType $ units dungeonState Map.! unitId
            isEnemyGround = any $ (/= friendlyType) . unitType . (units dungeonState Map.!)
            dungeonGraph = makeGraph dungeonState
            unitGround = Graph.keys $ Graph.filter (unitId `Set.member`) dungeonGraph

-- distance :: DungeonState -> Position -> Position -> Int
-- distance dungeonState source destination = abs (fst source - fst destination) + abs (snd source - snd destination)

makeGraph :: DungeonState -> Graph Position (Set Id)
makeGraph dungeonState = foldl (flip Graph.addEdge) graphWithNodes edges
    where 
        graphWithNodes = foldl addWithNeighbours Graph.empty (ground dungeonState)
            where addWithNeighbours graph currentPosition = Graph.addNode currentPosition neighbourUnits graph
                    where neighbourUnits = Set.fromList $ Map.keys
                            $ Map.filter ((`elem` adjacentPositions currentPosition) . position)
                            $ units dungeonState
        edges = foldl addEdges [] (ground dungeonState)
            where addEdges otherEdges currentPosition = neighbourEdges ++ otherEdges
                    where neighbourEdges = zip (repeat currentPosition)
                            $ filter (`Set.member` ground dungeonState) 
                            $ (adjacentPositions currentPosition)

adjacentPositions :: Position -> [Position]
adjacentPositions position = [
            (fst position, snd position + 1), 
            (fst position, snd position - 1),
            (fst position + 1, snd position), 
            (fst position - 1, snd position)
        ]

parseState :: [String] -> DungeonState
parseState lines = Seq.foldlWithIndex foldLine emptyState (Seq.fromList lines)
    where foldLine state lineNumber line = mergeState state (parseLine state lineNumber line)
          emptyState = DungeonState Set.empty Map.empty
          mergeState a b = DungeonState mergedGroundMap mergedUnits
                where mergedGroundMap = Set.union (ground a) (ground b)
                      mergedUnits = Map.union (units a) (units b)

parseLine :: DungeonState -> Int -> String -> DungeonState
parseLine state lineNumber line = Seq.foldlWithIndex updateDungeon state (Seq.fromList line)
    where
        updateDungeon dungeonState columnNumber char
            | char == 'G' = dungeonState{units = addUnit dungeonState Goblin}
            | char == 'E' = dungeonState{units = addUnit dungeonState Elve}
            | char == '.' = dungeonState{ground = newGroundMap}
            | char == '#' = dungeonState 
            where addUnit state unitType = Map.insert (length (units state)) (newUnit unitType) (units state) 
                    where newUnit unitType = Unit unitType (lineNumber, columnNumber) 200
                  newGroundMap = Set.insert (lineNumber, columnNumber) (ground dungeonState)
           