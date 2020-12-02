main = do
    raw_input <- readFile "input/day02.txt"
    let input = map read (lines raw_input) :: [Int]
    -- let passwordPolicy = (parsePasswordPolicy . head . lines) raw_input
    print $ length $ filter validPassword $ map parseSledPasswordPolicy $ lines raw_input -- 524
    print $ length $ filter validPassword $ map parseTobogganPasswordPolicy $ lines raw_input -- 485

data PasswordPolicy = PasswordPolicy { password ::String, rule :: Rule } deriving (Show)
data Rule = SledRentalsRule { token ::Char, tokenMin ::Int, tokenMax ::Int } 
          | TobogganCorporateRule { token ::Char, tokenAtA ::Int, tokenAtB ::Int } deriving (Show)

--parseRule (minT:'-':maxT:' ':token:':':' ':password) = Rule { token = token, minT = (read minT), maxT = 2}

validPassword :: PasswordPolicy -> Bool
validPassword (PasswordPolicy password (SledRentalsRule token tokenMin tokenMax)) = tokenMin <= tokenCount && tokenCount <= tokenMax
    where tokenCount = (length . filter (token ==)) password
validPassword (PasswordPolicy password (TobogganCorporateRule token tokenAtA tokenAtB)) = (password !! (tokenAtA - 1) == token) /= (password !! (tokenAtB - 1) == token)

parseMin line = span (/= '-') line
parseMax line = span (/= ' ') line
parseToken (token:rest) = (token, rest)
parsePassword line = tail line

parseSledPasswordPolicy line = PasswordPolicy { password = parsedPassword, rule = SledRentalsRule { token = fst parsedToken, tokenMin = (read . fst) parsedMin, tokenMax = (read . fst) parsedMax } }
                    where parsedPassword = (parsePassword . tail . snd) parsedToken
                          parsedToken = (parseToken . tail . snd) parsedMax
                          parsedMax = (parseMax . tail . snd) parsedMin
                          parsedMin = parseMin line

parseTobogganPasswordPolicy line = PasswordPolicy { password = parsedPassword, rule = TobogganCorporateRule { token = fst parsedToken, tokenAtA = (read . fst) parsedMin, tokenAtB = (read . fst) parsedMax } }
                    where parsedPassword = (parsePassword . tail . snd) parsedToken
                          parsedToken = (parseToken . tail . snd) parsedMax
                          parsedMax = (parseMax . tail . snd) parsedMin
                          parsedMin = parseMin line
