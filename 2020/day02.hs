main = do
    raw_input <- readFile "input/day02.txt"
    let input = map read (lines raw_input) :: [Int]
    let passwordPolicy = (parsePasswordPolicy . head . lines) raw_input
    print $ length $ filter validPassword $ map parsePasswordPolicy $ lines raw_input

data PasswordPolicy = PasswordPolicy { password ::String, rule :: Rule } deriving (Show)
data Rule = Rule { token ::Char, tokenMin ::Int, tokenMax ::Int } deriving (Show)

--parseRule (minT:'-':maxT:' ':token:':':' ':password) = Rule { token = token, minT = (read minT), maxT = 2}

validPassword :: PasswordPolicy -> Bool
validPassword (PasswordPolicy password (Rule token tokenMin tokenMax)) = tokenMin <= tokenCount && tokenCount <= tokenMax
    where tokenCount = (length . filter (token ==)) password

parseMin line = span (/= '-') line
parseMax line = span (/= ' ') line
parseToken (token:rest) = (token, rest)
parsePassword line = tail line

parsePasswordPolicy line = PasswordPolicy { password = parsedPassword, rule = Rule { token = fst parsedToken, tokenMin = (read . fst) parsedMin, tokenMax = (read . fst) parsedMax } }
                    where parsedPassword = (parsePassword . tail . snd) parsedToken
                          parsedToken = (parseToken . tail . snd) parsedMax
                          parsedMax = (parseMax . tail . snd) parsedMin
                          parsedMin = parseMin line
