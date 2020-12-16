import Text.ParserCombinators.Parsec
import Data.Either (rights)
import Data.List (concat, find, findIndex, delete, isPrefixOf, elemIndex)
import Data.Maybe (fromJust)

import Debug.Trace

main = do
    raw_input <- lines <$> readFile "input/day16.txt"
    -- raw_input <- lines <$> readFile "input/day16.example.txt"
    -- raw_input <- lines <$> readFile "input/day16.example2.txt"
    let raw_field_rules:(_:raw_my_ticket:[]):(_:raw_nearby_tickets):[] = splitOn "" raw_input
    
    let field_rules = rights $ map (parse parseFieldRule "") raw_field_rules
    let my_ticket = map read $ splitOn ',' raw_my_ticket :: [Int]
    let nearby_tickets = map (map read . splitOn ',') raw_nearby_tickets :: [[Int]]
    
    -- print my_ticket
    -- print nearby_tickets

    -- print $ parse parseFieldRule "" "departure location: 36-269 or 275-973"
    
    print $ sum $ filter (obviouslyInvalidValue field_rules) (concat nearby_tickets) -- 22073
    
    let validTickets = filter (not . (obviouslyInvalidTicket field_rules)) nearby_tickets

    print $ map (length . (validRulesAtIndex field_rules validTickets)) [0..(length field_rules) -1]

    -- let prevRuleNames = ["arrival station", "arrival track","duration","price","route","row","seat","wagon","zone"]
    let prevRuleNames = ["arrival track","duration","price","route","row","seat","wagon","zone"]
    let remainingRules = filter (\FieldRule{fieldName=name} -> name `elem` prevRuleNames) field_rules

    print $ map (validRulesAtIndex field_rules validTickets) [0..(length (head validTickets)) -1]

    let fieldOrder = detectFieldLocations field_rules validTickets
    let departureFieldIndexes = map fst $ filter (\(i, FieldRule{fieldName=name}) -> "departure" `isPrefixOf` name) fieldOrder

    print $ product $ map (my_ticket !!) departureFieldIndexes -- 1346570764607

    print "done"
    


data FieldRule = FieldRule { fieldName::String, rangeA::(Int, Int), rangeB::(Int, Int) } deriving (Show, Eq)

-- detectFieldLocations :: [FieldRule] -> [[Int]] -> [FieldRule]
-- detectFieldLocations :: [FieldRule] -> [[Int]] -> [FieldRule]
-- detectFieldLocations fieldRules tickets | trace (show (length fieldRules)) False = undefined
-- detectFieldLocations fieldRules tickets | trace (show 1) False = undefined

-- detectFieldLocations [] tickets = []
-- detectFieldLocations fieldRules tickets = ruleAtCurrentIndex:(detectFieldLocations (delete ruleAtCurrentIndex fieldRules) tickets)
--     where ruleAtCurrentIndex = fromJust $ find allValuesAtIndexValidForRule fieldRules
--           allValuesAtIndexValidForRule fieldRule = all (valueInsideBounds fieldRule) valuesAtIndex
--           valuesAtIndex = map (!! currentIndex) tickets
--           currentIndex = totalFields - length fieldRules
--           totalFields = length (head tickets)

detectFieldLocations :: [FieldRule] -> [[Int]] -> [(Int, FieldRule)]
detectFieldLocations [] tickets = []
detectFieldLocations fieldRules tickets = (nextFieldIndex, nextRule):(detectFieldLocations (delete nextRule fieldRules) tickets)
    where nextRule = head $ validRulesAtIndexes !! nextFieldIndex
          nextFieldIndex = fromJust $ findIndex ((==1) . length) validRulesAtIndexes
          validRulesAtIndexes = map (validRulesAtIndex fieldRules tickets) [0..totalFields-1]
          totalFields = length (head tickets)

validRulesAtIndex fieldRules tickets i = filter allValuesAtIndexValidForRule fieldRules
    where allValuesAtIndexValidForRule fieldRule = all (valueInsideBounds fieldRule) valuesAtIndex
          valuesAtIndex = map (!! i) tickets

obviouslyInvalidTicket :: [FieldRule] -> [Int] -> Bool
obviouslyInvalidTicket fieldRules ticketValues = any (obviouslyInvalidValue fieldRules) ticketValues

obviouslyInvalidValue :: [FieldRule] -> Int -> Bool
obviouslyInvalidValue fieldRules fieldValue = all (not . ((flip valueInsideBounds) fieldValue)) fieldRules

valueInsideBounds :: FieldRule -> Int -> Bool
valueInsideBounds FieldRule{rangeA=(rangeAMin, rangeAMax), rangeB=(rangeBMin, rangeBMax)} fieldValue = 
    (rangeAMin <= fieldValue && fieldValue <= rangeAMax) 
    || (rangeBMin <= fieldValue && fieldValue <= rangeBMax)

parseFieldRule :: Parser FieldRule
parseFieldRule = do
        fieldName <- many1 (noneOf ":")
        string ": "
        rangeAMin <- read <$> many1 digit
        char '-'
        rangeAMax <- read <$> many1 digit
        string " or "
        rangeBMin <- read <$> many1 digit
        char '-'
        rangeBMax <- read <$> many1 digit
        return (FieldRule {fieldName = fieldName, rangeA = (rangeAMin, rangeAMax) , rangeB = (rangeBMin, rangeBMax)})

splitOn delimiter xs = case break (== delimiter) xs of
    (y, _ : ys) -> y : splitOn delimiter ys
    (y, []) -> [y]
