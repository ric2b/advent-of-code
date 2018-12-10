import qualified Data.Map as Map
import Data.List
import Data.Maybe

import Debug.Trace

type Step = Char
type StepDependencies = Map.Map Step [Step]

data Dependency = Dependency {
    stepName :: Step,
    dependsOn :: Step
} deriving (Show, Eq)

parseDependencies :: String -> [Dependency]
parseDependencies input = [Dependency (head (line !! 7)) (head (line !! 1)) | line <- map words (lines input)]

toEdgeMap :: StepDependencies -> [Dependency] -> StepDependencies
toEdgeMap edgeMap [] = edgeMap
toEdgeMap edgeMap (current:rest) = toEdgeMap updatedMap rest
    where updatedMap = Map.insertWith (++) (stepName current) [dependsOn current] edgeMap

availableSteps :: StepDependencies -> [Step]    
availableSteps edgeMap = sort $ nub $ emptyKeys ++ (nub (concat (Map.elems edgeMap)) \\ Map.keys cleanEdgeMap)
    where
        emptyKeys = Map.keys (Map.filter (== []) edgeMap)
        cleanEdgeMap = Map.filter (/= []) edgeMap

nextStep :: StepDependencies -> Step    
nextStep edgeMap = head $ availableSteps edgeMap

removeDeps :: [Step] -> StepDependencies -> StepDependencies
removeDeps steps edgeMap = Map.map (filter (`notElem` steps)) $ Map.filterWithKey (\s _ -> s `notElem` steps) edgeMap

removeDep :: Step -> StepDependencies -> StepDependencies
removeDep step edgeMap = removeDeps [step] edgeMap

part1 edgeMap
    | edgeMap == Map.empty = []
    | otherwise = (nextStep edgeMap) : part1 (removeDep (nextStep edgeMap) edgeMap)

nWorkers = 5
defaultDelay = 60

type Time = Int
type Task = (Step, Time)

delay :: Step -> Time
delay step = defaultDelay + (fromJust $ step `elemIndex` ['A'..'Z'])

nextT :: Time -> [Task] -> Time
nextT t tasks
        | tasks == [] = t
        | otherwise =  foldr1 min $ map snd tasks

stepNotInTasks :: [Task] -> Step -> Bool
stepNotInTasks tasks step = step `notElem` map fst tasks

nextTasks :: Time -> [Task] -> StepDependencies -> [Task]
nextTasks t currentTasks dependencies = incompleteTasks ++ take (nWorkers - (length incompleteTasks)) newTasks
    where 
        incompleteTasks = filter (\task -> snd task > t) currentTasks
        newTasks
            | otherwise = map (\step -> (step, t + 1 + delay step)) $ filter (stepNotInTasks currentTasks) $ availableSteps dependencies

doneSteps :: Time -> [Task] -> [Step]
doneSteps t tasks = map fst $ filter (\task -> snd task <= t) tasks

ticker :: Time -> [Task] -> StepDependencies -> Int
ticker t currentTasks stepDependencies
    | stepDependencies == Map.empty = t + 1
    | otherwise = ticker nextT' nextTasks' nextDependencies'
        where 
            nextT' = nextT t $ nextTasks t currentTasks $ removeDeps (map fst currentTasks) stepDependencies
            nextTasks' = nextTasks t currentTasks $ removeDeps (doneSteps t currentTasks) stepDependencies
            nextDependencies' = removeDeps (doneSteps t currentTasks) stepDependencies  

part2 :: StepDependencies -> Int     
part2 edgeMap = ticker (-1) [] edgeMap       
-- BHTUMOFLQZCPWKIVNRXASJDYEG 876

main :: IO() 
main = do
    input <- readFile "input/day7.txt"
    let edgeMap = toEdgeMap Map.empty $ parseDependencies input
    print $ part1 edgeMap
    print $ part2 edgeMap
