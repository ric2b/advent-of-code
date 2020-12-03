import Text.ParserCombinators.Parsec

main = do
    input <- lines <$> readFile "input/day02.txt"
    print $ countValid SledRentalsPolicy input -- 524
    print $ countValid TobogganCorporatePolicy input -- 485

data PasswordAndPolicy = PasswordAndPolicy { password ::String, policy :: Policy } deriving (Show)
data Policy = SledRentalsPolicy { token ::Char, tokenMin ::Int, tokenMax ::Int } 
            | TobogganCorporatePolicy { token ::Char, indexA ::Int, indexB ::Int } deriving (Show)

countValid policyConstructor = length . (filter validEntry) . (map (parse (parsePasswordAndPolicy policyConstructor) ""))

validEntry :: Either ParseError PasswordAndPolicy -> Bool
validEntry (Left _) = False
validEntry (Right passwordAndPolicy) = validPassword passwordAndPolicy

validPassword :: PasswordAndPolicy -> Bool
validPassword (PasswordAndPolicy password (SledRentalsPolicy token tokenMin tokenMax)) = tokenMin <= tokenCount && tokenCount <= tokenMax
    where tokenCount = (length . filter (token ==)) password
validPassword (PasswordAndPolicy password (TobogganCorporatePolicy token indexA indexB)) = tokenAtA /= tokenAtB
    where tokenAtA = password !! (indexA - 1) == token
          tokenAtB = password !! (indexB - 1) == token

parsePasswordAndPolicy :: (Char -> Int -> Int -> Policy) -> Parser PasswordAndPolicy
parsePasswordAndPolicy policyConstructor = do
    paramA <- read <$> many1 digit
    char '-'
    paramB <- read <$> many1 digit
    char ' '
    token <- anyChar
    string ": "
    password <- many1 anyChar
    return (PasswordAndPolicy password (policyConstructor token paramA paramB))
