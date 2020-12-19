import qualified Data.Map as M
import qualified Text.ParserCombinators.ReadP as P
import Data.Char (isDigit)

main = do
    rawInput <- lines <$> readFile "input/day19.txt"
    let (raw_rules, _:messages) = break (== "") rawInput

    let rules = M.fromList $ map (parse (ruleWithId <* P.eof)) raw_rules
    print $ length $ filter (validMessage (makeParser rules (See 0) <* P.eof)) messages -- 220

    let newRules = M.fromList $ map (parse (ruleWithId <* P.eof)) ["8: 42 | 42 8", "11: 42 31 | 42 11 31"]
    let changedRules = M.union newRules rules
    print $ length $ filter (validMessage (makeParser changedRules (See 0) <* P.eof)) messages -- 439

validMessage :: P.ReadP Char -> String -> Bool
validMessage parser message = case P.readP_to_S parser message of
    [(_, "")] -> True
    otherwise -> False

parse :: P.ReadP a -> String -> a
parse parser = fst . head . P.readP_to_S parser

data Rule = Letter Char | Sequence [Rule] | Choice [Rule] | See RuleId deriving (Show, Eq, Read, Ord)
type RuleId = Int

makeParser :: M.Map RuleId Rule -> Rule -> P.ReadP Char
makeParser _ (Letter c) = P.char c
makeParser rules (Sequence [r]) = makeParser rules r
makeParser rules (Sequence (r:rs)) = makeParser rules r >> makeParser rules (Sequence rs)
makeParser rules (Choice [r]) = makeParser rules r
makeParser rules (Choice (r:rs)) = makeParser rules r P.+++ makeParser rules (Sequence rs)
makeParser rules (See ruleId) = makeParser rules (rules M.! ruleId)

-- parsing

ruleWithId :: P.ReadP (RuleId, Rule)
ruleWithId = do
    currentRuleId <- read <$> P.many1 (P.satisfy isDigit)
    P.char ':'
    P.skipSpaces
    rule <- letter P.+++ choice
    return (currentRuleId, rule)

letter :: P.ReadP Rule
letter = Letter <$> P.between (P.char '"') (P.char '"') P.get

choice :: P.ReadP Rule
choice = Choice <$> P.sepBy1 sequential (P.skipSpaces *> P.char '|' <* P.skipSpaces)

sequential :: P.ReadP Rule
sequential = Sequence <$> P.sepBy1 see (P.char ' ')

see :: P.ReadP Rule
see = See <$> read <$> P.many1 (P.satisfy isDigit)
