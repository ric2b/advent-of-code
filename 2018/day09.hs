import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.IntMap.Strict as IntMap

main :: IO() 
main = do
    input <- words <$> readFile "input/day9.txt"
    let nPlayers = read $ input !! 0 ::Int
    let maxMarble = read $ input !! 6 ::Int
    print "I didn't write this, taken from reddit because my original solution was awfully slow :/"
    print $ playGame nPlayers maxMarble
    print $ playGame nPlayers (maxMarble * 100)


placeIndex = 2
removeIndex = 7

isScoreMarble :: Int -> Bool
isScoreMarble i = i `rem` 23 == 0   

playGame :: Int {- ^ players -} -> Int {- ^ max marble -} -> Int {- ^ max score -}
playGame players marbles = loop IntMap.empty (Seq.singleton 0) 1
  where
    loop scores circle i
      | i > marbles = maximum scores
      | isScoreMarble i =
          case rotate (-removeIndex) circle of
            Seq.Empty              -> error "game: empty circle"
            picked Seq.:<| circle' -> loop scores' circle' (i+1)
              where
                scores' = IntMap.insertWith (+) (i `rem` players) (i + picked) scores
      | otherwise = loop scores (i Seq.<| rotate placeIndex circle) (i+1)

rotate :: Int -> Seq a -> Seq a
rotate n xs
  | Seq.null xs = xs
  | otherwise   = case Seq.splitAt (n `mod` Seq.length xs) xs of
                    (l, r) -> r Seq.>< l

                 