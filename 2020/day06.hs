import qualified Data.Set as Set

main = do
    input <- lines <$> readFile "input/day06.txt"
    let groups = splitOn "" input
    print $ sum $ map Set.size $ map positiveAnswersByAnyoneInGroup groups -- 6947
    print $ sum $ map Set.size $ map positiveAnswersByEntireGroup groups -- 3398

positiveAnswersByAnyoneInGroup groupAnswers = Set.unions (map positiveAnswersByPerson groupAnswers)
positiveAnswersByEntireGroup groupAnswers = foldl1 Set.intersection (map positiveAnswersByPerson groupAnswers)

positiveAnswersByPerson personAnswers = Set.fromList personAnswers

splitOn delimiter xs = case break (== delimiter) xs of
    (y, _ : ys) -> y : splitOn delimiter ys
    (y, []) -> [y]
