import qualified Data.Set as Set
import Data.List

data Claim = Claim {
    claimId :: Int,
    xCoordinate :: Int,
    yCoordinate :: Int,
    width :: Int,
    height :: Int
} deriving (Show, Ord, Eq)

splitOn :: String -> String -> [String]
splitOn [] _ = []
splitOn string@(first:rest) splitters
    | first `elem` splitters = splitOn rest splitters
    | otherwise = [takeWhile (`notElem` splitters) string] 
                    ++ splitOn (dropWhile (`notElem` splitters) rest) splitters

parseClaim [id, x, y, width, height] = Claim id x y width height

leftEdge claim = xCoordinate claim
rightEdge claim = xCoordinate claim + width claim - 1
topEdge claim = yCoordinate claim
bottomEdge claim = yCoordinate claim + height claim - 1

overlaps :: Claim -> Claim -> Bool
overlaps claim1 claim2
    | rightEdge claim1 < leftEdge claim2 = False
    | leftEdge claim1 > rightEdge claim2 = False
    | bottomEdge claim1 < topEdge claim2 = False
    | topEdge claim1 > bottomEdge claim2 = False
    | otherwise = True

overlappingClaims claims = [(claim1, claim2) | (claim1:rest) <- tails claims, claim2 <- rest, claim1 `overlaps` claim2]    

overlappingPoints :: (Claim, Claim) -> [(Int, Int)]
overlappingPoints (claim1, claim2) = [(x, y) | x <- [left .. right], y <- [top .. bottom]]
    where left = max (leftEdge claim1) (leftEdge claim2)
          right = min (rightEdge claim1) (rightEdge claim2)
          top = max (topEdge claim1) (topEdge claim2)
          bottom = min (bottomEdge claim1) (bottomEdge claim2) 
          
unpackClaimTuple :: (Claim, Claim) -> [Claim]
unpackClaimTuple (claim1, claim2) = claim1:claim2:[]

part1 :: [Claim] -> Int
part1 claims = length $ Set.fromList $ concat $ map overlappingPoints (overlappingClaims claims)    

part2 :: [Claim] -> Int
part2 claims = claimId $ Set.findMin $ (Set.fromList claims) `Set.difference` (Set.fromList (concat (map unpackClaimTuple (overlappingClaims claims))))

main :: IO ()
main = do
    input <- readFile "input/day3.txt"
    let claims = [parseClaim . map read $ splitOn line "# @,:x" | line <- (lines input)]
    print $ part1 claims
    print $ part2 claims