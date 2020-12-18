import Text.ParserCombinators.ReadP
import Data.Char (isDigit)

main = do
    rawInput <- lines <$> readFile "input/day18.txt"

    print $ sum $ map (parse (leftToRight <* eof)) rawInput -- 30753705453324
    print $ sum $ map (parse (addPrecedence <* eof)) rawInput -- 244817530095503

parse parser = fst . head . readP_to_S parser

integer :: ReadP Int 
integer = read <$> many1 (satisfy isDigit)

leftToRight :: ReadP Int
leftToRight = expr
    where expr = factor `chainl1` op
          factor = (between (char '(') (char ')') expr) +++ integer
          op = ((+) <$ (skipSpaces *> char '+' <* skipSpaces)) +++ ((*) <$ (skipSpaces *> char '*' <* skipSpaces))

addPrecedence :: ReadP Int
addPrecedence = expr
    where expr = term `chainl1` ((*) <$ (skipSpaces *> char '*' <* skipSpaces))
          term = factor `chainl1` ((+) <$ (skipSpaces *> char '+' <* skipSpaces))
          factor = (between (char '(') (char ')') expr) +++ integer
