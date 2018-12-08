import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Data.Ord

shiftAction = "Guard"
sleepAction = "falls"
wakeAction = "wakes"

type GuardId = Int 

data Event = Event {
    guardId :: Maybe GuardId,
    date :: String,
    hours :: Int,
    minutes :: Int,
    action :: String
} deriving (Show, Eq)

instance Ord Event where
    event1 `compare` event2 = timestamp event1 `compare` timestamp event2
        where timestamp event = (date event, hours event, minutes event) 

splitEvent event = words $ filter (`notElem` "[]") event

parseEvent eventWords = Event guardId date hours minutes action
    where guardId 
            | length eventWords == 6 = Just $ read $ filter (/='#') $ eventWords !! 3 
            | otherwise = Nothing
          date = eventWords !! 0
          hours = read $ takeWhile (/=':') $ eventWords !! 1
          minutes = read $ tail $ dropWhile (/=':') $ eventWords !! 1
          action = eventWords !! 2       

setId :: GuardId -> Event -> Event
setId id event = Event (Just id) (date event) (hours event) (minutes event) (action event)

addGuardIds :: Maybe GuardId -> [Event] -> [Event]   
addGuardIds _ [] = []
addGuardIds Nothing (currentEvent:rest) = currentEvent : addGuardIds (guardId currentEvent) rest
addGuardIds (Just currentGuard) (currentEvent:rest) 
    | action currentEvent == shiftAction = currentEvent : addGuardIds (guardId currentEvent) rest
    | otherwise = setId currentGuard currentEvent : addGuardIds (Just currentGuard) rest

type Minute = Int

makePatrolLogs :: Maybe Minute -> [Event] -> Map.Map GuardId [Minute] -> Map.Map GuardId [Minute]
makePatrolLogs _ [] map = map
makePatrolLogs minute (event:rest) map
    | action event == shiftAction = makePatrolLogs Nothing rest map
    | action event == sleepAction = makePatrolLogs (Just (minutes event)) rest map
    | action event == wakeAction = case ((guardId event), minute) of 
        (Just currentGuard, Just minute) -> 
            makePatrolLogs Nothing rest $ Map.insertWith (++) currentGuard [minute .. (minutes event)] map
        otherwise -> error $ "wut: " ++ (show event)

sleepiestGuard :: Map.Map GuardId [Minute] -> (GuardId, [Minute])
sleepiestGuard patrolLogs = Map.foldrWithKey sleepiest (0, []) patrolLogs
            where sleepiest guardId1 guardMinutes1 guard2 = 
                    if (length guardMinutes1) > (length (snd guard2))
                    then (guardId1, guardMinutes1) 
                    else guard2

minuteFrequencies :: [Minute] -> Map.Map Minute Int
minuteFrequencies guardMinutes = Map.fromListWith (+) [(m, 1) | m <- guardMinutes] 

mostFrequent :: [Minute] -> (Minute, Int)
mostFrequent guardMinutes = (Map.foldrWithKey folder (0, 0) (minuteFrequencies guardMinutes))
                        where folder k a b = if a >= snd b then (k, a) else b

maxGuardFrequencies :: Map.Map GuardId [Minute] -> Map.Map GuardId (Minute, Int)
maxGuardFrequencies guardFrequencyLogs = Map.map mostFrequent guardFrequencyLogs

part1 :: (GuardId, [Minute]) -> Int
part1 (guardId, guardMinutes) = guardId * fst (mostFrequent guardMinutes)

part2 :: Map.Map GuardId [Minute] -> Int
part2 guardFrequencyLogs = resulter $ Map.foldrWithKey folder (0, (0, 0)) (maxGuardFrequencies guardFrequencyLogs)
                        where 
                            folder k a b = if snd a >= snd (snd b) then (k, a) else b
                            resulter (currentGuard, (minute, count)) = currentGuard * minute

main :: IO() 
main = do
    input <- readFile "input4.txt"
    let orderedEvents = sort $ map (parseEvent . splitEvent) (lines input)
    print $ part1 $ sleepiestGuard $ makePatrolLogs Nothing (addGuardIds Nothing orderedEvents) Map.empty
    print $ part2 $ makePatrolLogs Nothing (addGuardIds Nothing orderedEvents) Map.empty
