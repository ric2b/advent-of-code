import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Char (isHexDigit)

main = do
    input <- lines <$> readFile "input/day04.txt"
    let passports = toPassports input
    print $ length $ filter hasMandatoryFields passports -- 264
    print $ length $ filter validPassport passports -- 224

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
validPassport passport = (hasMandatoryFields passport) && (fieldsAreValid passport)

fieldsAreValid :: Passport -> Bool
fieldsAreValid passport = List.all validField (Map.toList passport)

validField :: (String, String) -> Bool
validField ("byr", value) = 1920 <= birth_year && birth_year <= 2002 where birth_year = read value
validField ("iyr", value) = 2010 <= issue_year && issue_year <= 2020 where issue_year = read value
validField ("eyr", value) = 2020 <= expiration_year && expiration_year <= 2030 where expiration_year = read value
validField ("hgt", value) = case reads value :: [(Integer, String)] of 
    [(size, "cm")] -> 150 <= size && size <= 193
    [(size, "in")] -> 59 <= size && size <= 76
    _ -> False
validField ("hcl", (x:xs)) = x == '#' && length xs == 6 && List.all isHexDigit xs
validField ("ecl", value) = value `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
validField ("pid", value) = length value == 9 && case reads value :: [(Int, String)] of
    [(number, _)] -> True
    _ -> False
validField ("cid", _) = True
validField _ = True

hasMandatoryFields :: Passport -> Bool
hasMandatoryFields passport = mandatoryFields `Set.isSubsetOf` Map.keysSet passport

toPassports :: [String] -> [Passport]
toPassports lines = map parse_passport raw_passports
    where parse_passport raw_passport = Map.fromList $ map (to2tuple . (splitOn ':')) raw_passport
          to2tuple [x,y] = (x, y)
          raw_passports = map (words . unwords) $ splitOn "" lines
          
splitOn delimiter xs = case break (== delimiter) xs of
    (y, _ : ys) -> y : splitOn delimiter ys
    (y, []) -> [y]
