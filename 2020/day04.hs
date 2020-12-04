import qualified Data.Map as Map
import qualified Data.Set as Set

main = do
    input <- lines <$> readFile "input/day04.txt"
    let passports = toPassports input
    print $ length $ filter validPassport passports -- 264

type Passport = Map.Map String String
mandatoryFields = Set.fromList [
        "byr", -- (Birth Year)
        "iyr", -- (Issue Year)
        "eyr", -- (Expiration Year)
        "hgt", -- (Height)
        "hcl", -- (Hair Color)
        "ecl", -- (Eye Color)
        "pid"--, -- (Passport ID)
        --"cid"  -- (Country ID) We don't really need this ;)
    ]

validPassport :: Passport -> Bool
validPassport passport = mandatoryFields `Set.isSubsetOf` Map.keysSet passport

toPassports :: [String] -> [Passport]
toPassports lines = map parse_passport raw_passports
    where parse_passport raw_passport = Map.fromList $ map (to2tuple . (splitOn ':')) raw_passport
          to2tuple [x,y] = (x,y)
          raw_passports = map (words . unwords) $ splitOn "" lines
          
splitOn delimiter xs = case break (== delimiter) xs of
    (y, _ : ys) -> y : splitOn delimiter ys
    (y, []) -> [y]
