module Graph (
    empty,
    Graph.null,
    fromEdgeList,
    addNode,
    -- adjust,
    addEdge,
    -- member,
    Graph.lookup,
    (!),
    Graph.filter,
    keys,
    neighbours,
    removeNode,
    removeEdge,
    reachable,
    reachableWith,
    shortestPaths,
    -- Instruction(..),
    Graph
) where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.List (minimumBy)

data Graph k d = Graph {
    nodes :: Map k d,
    edges :: Map k (Set k)
} deriving (Eq, Show)

infixl ! --, !?

empty :: Graph k d 
empty = Graph Map.empty Map.empty

null :: (Eq k, Eq d) => Graph k d -> Bool
null graph = graph == empty

-- member :: Ord k => k -> Graph k d -> Bool 
-- member nodeKey graph = Map.member nodeKey (nodes graph)

lookup :: Ord k => k -> Graph k d -> Maybe d
lookup nodeKey graph = Map.lookup nodeKey (nodes graph)

-- (!?) :: Ord k => k -> Graph k d -> Maybe d
-- (!?) = Graph.lookup

(!) :: Ord k => Graph k d -> k -> d
(!) graph nodeKey = (nodes graph) Map.! nodeKey

addNode :: Ord k => k -> d -> Graph k d -> Graph k d
addNode nodeKey nodeData graph
    | Map.member nodeKey (nodes graph) = graph{nodes = newNodes}
    | otherwise = graph{nodes = newNodes, edges = newEdges}
        where newNodes = Map.insert nodeKey nodeData (nodes graph)
              newEdges = Map.insert nodeKey Set.empty (edges graph)

-- adjust :: Ord k => (d -> d) -> k -> Graph k d -> Graph k d 
-- adjust f nodeKey graph = graph{nodes = Map.adjust f nodeKey (nodes graph)}

-- dangerous, delete edges first
fastRemoveNode :: Ord k => k -> Graph k d -> Graph k d
fastRemoveNode nodeKey graph = graph{nodes = Map.delete nodeKey (nodes graph)}

-- dangerous, add nodes first
addEdge :: Ord k => (k, k) -> Graph k d -> Graph k d
addEdge (keyA, keyB) graph
    | not (Map.member keyA (nodes graph) && Map.member keyB (nodes graph)) = error "Node doesn't exist"
    | otherwise = 
        graph{edges = Map.adjust (Set.insert keyA) keyB $ Map.adjust (Set.insert keyB) keyA (edges graph)}
    
removeEdge :: Ord k => (k, k) -> Graph k d -> Graph k d
removeEdge (keyA, keyB) graph = 
    graph{edges = Map.adjust (Set.delete keyA) keyB $ Map.adjust (Set.delete keyA) keyA (edges graph)}

removeNode :: Ord k => k -> Graph k d -> Graph k d
removeNode nodeKey graph = fastRemoveNode nodeKey graph{edges = newEdges}
    where newEdges = Map.delete nodeKey $ Map.map (Set.delete nodeKey) (edges graph)

fromEdgeList :: Ord k => [(k, k)] -> Graph k (Maybe String)
fromEdgeList edgeList = foldl addNodesAndEdge empty edgeList
    where addNodesAndEdge graph (keyA, keyB) = addEdge (keyA, keyB) 
            $ addNode keyA Nothing $ addNode keyB Nothing graph

filter :: Ord k => (d -> Bool) -> Graph k d -> Graph k d
filter filterFunction graph = foldl (flip removeNode) graph 
    $ Map.keys $ Map.filter (not . filterFunction) (nodes graph)

keys :: Ord k => Graph k d -> [k]
keys = Map.keys . nodes

neighbours :: Ord k => Graph k d -> k -> Set k
neighbours graph nodeKey = edges graph Map.! nodeKey 

bfs :: Ord k => Set k -> Set k -> Graph k d -> Set k
bfs queue reached graph
    | Set.null queue = reached
    | otherwise = bfs newQueue (Set.union reached queue) graph
        where newQueue = Set.filter (`Set.notMember` reached) 
                $ foldl1 Set.union 
                $ Set.map (neighbours graph) queue             

reachableWith :: Ord k => (d -> Bool) -> k -> Graph k d -> Set k
reachableWith filterFunction source graph = Set.filter (filterFunction . (graph !))
    $ bfs (Set.singleton source) Set.empty graph

reachable :: Ord k => k -> Graph k d -> Set k
reachable = reachableWith (\_ -> True)

-- bfsPaths :: (Ord k, Eq d) => Graph k d -> [k] -> k -> k -> [[k]]
-- bfsPaths graph parents target source
--     | Graph.null graph = []
--     | target `Set.member` neighbours graph source = [reverse (source:parents)]
--     | otherwise = concatMap (bfsPaths restOfGraph (source:parents) target) $ neighbours graph source
--         where restOfGraph = removeNode source graph

bfsPaths :: (Ord k, Eq d) => Graph k d -> [k] -> k -> k -> [[k]]
bfsPaths graph parents target source
    | Graph.null graph = []
    | target `Set.member` neighbours graph source = [reverse (source:parents)]
    | otherwise = concatMap stuff $ neighbours graph source
        where stuff nodeKey = bfsPaths restOfGraph (source:parents) target nodeKey
                where   restOfGraph = foldl (flip removeNode) (removeNode source graph) otherNeighbours
                        otherNeighbours = Set.filter (/= nodeKey) $ neighbours graph source

shortestPaths :: (Ord k, Eq d) => k -> k -> Graph k d -> [[k]]
shortestPaths source target graph
    | target `Set.notMember` reachable source graph = []
    | otherwise = Prelude.filter ((minimum (map length paths) ==) . length) paths
        where paths = bfsPaths graph [] target source

main = do 
    let graph = fromEdgeList [('a', 'b'), ('b', 'c'), ('b', 'd'), ('c', 'e'), ('d', 'j'), ('j', 'e'), ('k', 'l')]
    print graph
    -- print $ neighbours 'a' $ removeNode 'c' graph
    print $ reachable 'a' graph
    print $ reachable 'e' graph
    print $ bfsPaths graph [] 'e' 'a'
    print $ shortestPaths 'a' 'e' graph
