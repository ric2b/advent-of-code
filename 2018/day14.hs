import Data.Char
import Data.List (tails, isPrefixOf, isInfixOf, iterate)
import Data.Foldable (toList)
import qualified Data.Sequence as Seq

input = 598701
-- input = 92510

seqInput = map digitToInt $ show input
nElves = 2

main = do
    let iterations = iterate updateScoreboard (Seq.fromList [3, 7], [0..nElves-1])
    let part1iteration = head $ dropWhile ((< 10 + input) . Seq.length . fst) iterations
    print $ concat $ map show $ take 10 $ drop input $ toList $ fst part1iteration

type Score = Int
type Position = Int

updateScoreboard :: (Seq.Seq Score, [Position]) -> (Seq.Seq Score, [Position])
updateScoreboard (scoreboard, positions) = (scoreboard Seq.>< (combineRecipes $ Seq.fromList (map (scoreboard `Seq.index`) newPositions)), newPositions)
    where newPositions = map (updatePosition scoreboard) positions

updatePosition :: Seq.Seq Score -> Position -> Position
updatePosition scoreboard currentPosition = (currentPosition + 1 + currentScore) `mod` (Seq.length scoreboard)
    where currentScore = scoreboard `Seq.index` currentPosition

combineRecipes :: Seq.Seq Score -> Seq.Seq Score
combineRecipes = Seq.fromList . (map digitToInt) . show . (foldl1 (+))
