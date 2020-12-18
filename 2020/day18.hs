import Text.ParserCombinators.ReadP
import Data.Char (isDigit)

main = do
    rawInput <- lines <$> readFile "input/day18.txt"

    print $ sum $ map (parse (leftToRight <* eof)) rawInput -- 30753705453324
    print $ sum $ map (parse (addPrecedence <* eof)) rawInput -- 244817530095503

parse parser = fst . head . readP_to_S parser

leftToRight :: ReadP Int
leftToRight = expr
    where expr = factor `chainl1` (add +++ mult)
          factor = (betweenParens expr) +++ integer

addPrecedence :: ReadP Int
addPrecedence = expr
    where expr = term `chainl1` mult
          term = factor `chainl1` add
          factor = (betweenParens expr) +++ integer

betweenParens :: ReadP a -> ReadP a
betweenParens = between (char '(') (char ')')

add, mult :: ReadP (Int -> Int -> Int)
add = ((+) <$ (skipSpaces *> char '+' <* skipSpaces))
mult = ((*) <$ (skipSpaces *> char '*' <* skipSpaces))

integer :: ReadP Int 
integer = read <$> many1 (satisfy isDigit)
