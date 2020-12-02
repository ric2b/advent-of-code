main = do
    input <- lines <$> readFile "input/day02.txt"
    print $ countValid SledRentalsPolicy input -- 524
    print $ countValid TobogganCorporatePolicy input -- 485

data PasswordAndPolicy = PasswordAndPolicy { password ::String, policy :: Policy } deriving (Show)
data Policy = SledRentalsPolicy { token ::Char, tokenMin ::Int, tokenMax ::Int } 
            | TobogganCorporatePolicy { token ::Char, indexA ::Int, indexB ::Int } deriving (Show)

countValid policyConstructor = length . (filter validPassword) . (map (parsePasswordAndPolicy policyConstructor))

validPassword :: PasswordAndPolicy -> Bool
validPassword (PasswordAndPolicy password (SledRentalsPolicy token tokenMin tokenMax)) = tokenMin <= tokenCount && tokenCount <= tokenMax
    where tokenCount = (length . filter (token ==)) password
validPassword (PasswordAndPolicy password (TobogganCorporatePolicy token indexA indexB)) = tokenAtA /= tokenAtB
    where tokenAtA = password !! (indexA - 1) == token
          tokenAtB = password !! (indexB - 1) == token

parsePasswordAndPolicy policyConstructor line = PasswordAndPolicy { password = parsedPassword, policy = policy }
    where policy = policyConstructor (fst parsedToken) ((read . fst) parsedParamA) ((read . fst) parsedParamB)
          parsedPassword = (tail . tail . snd) parsedToken
          parsedToken = ((head . tail . snd) parsedParamB, (tail . tail . snd) parsedParamB)
          parsedParamB = (span (/= ' ') . tail . snd) parsedParamA
          parsedParamA = span (/= '-') line
