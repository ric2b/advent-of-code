import qualified Data.IntMap as M

main = do
    let input = [9, 2, 5, 1, 7, 6, 8, 3, 4]
    let cups = M.fromList $ zip input (tail $ cycle input)

    print $ concatMap show . cupsToList . fst $ iterate move (cups, head input) !! 100 -- 69852437

    let biggerInput = input ++ [(maximum input)+1 .. (10^6)]
    let biggerCups =  M.fromList $ zip biggerInput (tail $ cycle biggerInput)

    print $ product . take 2 . cupsToList . fst $ iterate move (biggerCups, head biggerInput) !! (10^7) -- 91408386135

type Cup = Int
type NextCup = Int

cupsToList :: M.IntMap NextCup -> [Cup]
cupsToList cups = drop 1 $ take (M.size cups) $ iterate (cups M.!) 1

move :: (M.IntMap NextCup, Cup) -> (M.IntMap NextCup, Cup)
move (cups, currentCup) = (insertCups rest pickedUp destinationCup, nextCup)
    where nextCup = cups M.! (last pickedUp)
          destinationCup = head [x | x <- [currentCup-1, currentCup-2, currentCup-3, currentCup-4, maxCup, maxCup-1, maxCup-3], x > 0 && x `notElem` pickedUp]
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
