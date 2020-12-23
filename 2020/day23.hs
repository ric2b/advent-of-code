import qualified Data.IntMap as M
import Data.List (sort, (\\), nub)

import Debug.Trace

main = do
    let input = [9, 2, 5, 1, 7, 6, 8, 3, 4]
    -- let input = [3, 8, 9, 1, 2, 5, 4, 6, 7]
    print $ concat $ map show $ take (length input - 1) $ tail $ dropWhile (/= 1) $ cycle $ iterate move input !! 100 -- 69852437

    let cups = M.fromList $ zip input (tail $ cycle input)
    -- print $ (M.! 2) $ M.fromList $ zip input (tail $ cycle input)

    -- let (pickedUp, rest) = pickUp3Cups cups 3
    -- let result = insertCups rest pickedUp 2

    -- print $ result

    -- print $ cupsToList . fst $ iterate moveFaster (cups, head input) !! 0
    -- print $ cupsToList . fst $ iterate moveFaster (cups, head input) !! 1
    -- print $ iterate moveFaster (cups, head input) !! 1
    -- print $ cupsToList . fst $ iterate moveFaster (cups, head input) !! 2
    -- print $ iterate moveFaster (cups, head input) !! 2
    print $ concat . map show . cupsToList . fst $ iterate moveFaster (cups, head input) !! 100

    -- let bigInput = input ++ [(maximum input)+1..50]
    let biggerInput = input ++ [(maximum input)+1..1000000]
    let biggerCups =  M.fromList $ zip biggerInput (tail $ cycle biggerInput)
    -- print $ length $ nub $ take 10000 $ iterate move input
    -- mapM_ print $ take 10 $ iterate (moveFast (minimum bigInput, maximum bigInput)) bigInput
    print $ take 2 $ tail $ dropWhile (/= 1) $ cycle $ iterate (moveFast (maximum input)) input !! 100
    -- print $ take 2 $ tail $ dropWhile (/= 1) $ cycle $ iterate (moveFast (maximum biggerInput)) biggerInput !! 10000000
    -- print $ take 2 $ tail $ dropWhile (/= 1) $ cycle $ iterate (moveFast (maximum biggerInput)) biggerInput !! 1000
    print $ take 2 . cupsToList . fst $ iterate moveFaster (biggerCups, head biggerInput) !! 10000000 -- 91408386135

move :: [Int] -> [Int]
-- move cups | trace (show cups) False = undefined
move cups@(currentCup:otherCups) = take (length cups) $ tail $ dropWhile (/= currentCup) $ cycle withReplacedCups
    where withReplacedCups = prevCups ++ [destination] ++ pickedUp ++ nextCups
          (prevCups, destination:nextCups) = break (== destinationCup) restOfCups
          destinationCup = if currentCup == minimum restOfCups then maximum restOfCups else last . fst . break (== currentCup) . sort $ restOfCups
          restOfCups = cups \\ pickedUp
          pickedUp = take 3 otherCups

moveFast :: Int -> [Int] -> [Int]
-- moveFast _ _ | trace (show cups) False = undefined
moveFast maxCup cups@(currentCup:otherCups) = take (length cups) $ tail $ dropWhile (/= currentCup) $ cycle withReplacedCups
    where withReplacedCups = prevCups ++ destination:pickedUp ++ nextCups
          (prevCups, destination:nextCups) = break (== destinationCup) restOfCups
          destinationCup = head [x | x <- [currentCup-1,currentCup-2 .. currentCup-4] ++ [maxCup, maxCup-1..maxCup-3], x > 0 && x `notElem` pickedUp]
          restOfCups = filter (`notElem` pickedUp) cups
          pickedUp = take 3 otherCups

type Cup = Int
type NextCup = Int

cupsToList :: M.IntMap NextCup -> [Cup]
cupsToList cups = drop 1 $ take (M.size cups) $ iterate (cups M.!) 1
    
moveFaster :: (M.IntMap NextCup, Cup) -> (M.IntMap NextCup, Cup)
moveFaster (cups, currentCup) = (insertCups rest pickedUp destinationCup, nextCup)
    where nextCup = cups M.! (last pickedUp)
          destinationCup = head [x | x <- [currentCup-1,currentCup-2 .. currentCup-4] ++ [maxCup, maxCup-1..maxCup-3], x > 0 && x `notElem` pickedUp]
          maxCup = fst $ M.findMax cups
          (pickedUp, rest) = pickUp3Cups cups currentCup

insertCups :: M.IntMap NextCup -> [Cup] -> Cup -> M.IntMap NextCup
insertCups cups cupsToInsert afterCup = M.union insertedCups cups
    where insertedCups = M.fromList $ zip (afterCup:cupsToInsert) (cupsToInsert ++ [currentAfter])
          currentAfter = cups M.! afterCup

pickUp3Cups :: M.IntMap NextCup -> Cup -> ([Cup], M.IntMap NextCup)
pickUp3Cups cups currentCup = (next3Cups, withoutPickedCups)
    where withoutPickedCups = M.insert currentCup (cupNAfter 3) cups
          next3Cups = take 3 $ nextCups currentCup
          cupNAfter n = nextCups currentCup !! n
          nextCups cup = tail $ iterate nextCupOf cup
          nextCupOf cup = cups M.! cup

    -- Each move, the crab does the following actions:

    -- The crab picks up the three cups that are immediately clockwise of the current cup. They are removed from the circle; cup spacing is adjusted as necessary to maintain the circle.
    -- The crab selects a destination cup: the cup with a label equal to the current cup's label minus one. If this would select one of the cups that was just picked up, the crab will keep subtracting one until it finds a cup that wasn't just picked up. If at any point in this process the value goes below the lowest value on any cup's label, it wraps around to the highest value on any cup's label instead.
    -- The crab places the cups it just picked up so that they are immediately clockwise of the destination cup. They keep the same order as when they were picked up.
    -- The crab selects a new current cup: the cup which is immediately clockwise of the current cup.
