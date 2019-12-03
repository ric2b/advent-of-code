-- A header, which is always exactly two numbers:
--     The quantity of child nodes.
--     The quantity of metadata entries.
-- Zero or more child nodes (as specified in the header).
-- One or more metadata entries (as specified in the header).

-- 2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2
-- A----------------------------------
--     B----------- C-----------
--                      D-----

import Data.Tree (Tree(Node), rootLabel, subForest)

type Metadata = [Int]

parseTree :: ([Tree Metadata], [Int]) -> ([Tree Metadata], [Int])
parseTree (nodes, (nChildren:nMetadata:rest)) =
    let (children, remaining) = iterate parseTree ([], rest) !! nChildren
    in  (Node (take nMetadata remaining) (reverse children) : nodes, drop nMetadata remaining)

part1 :: Tree Metadata -> Int
part1 = sum . fmap sum

part2 :: Tree Metadata -> Int
part2 tree =
    if   null (subForest tree) then sum (rootLabel tree)
    else sum . map (part2 . (subForest tree !!) . subtract 1) . filter (<= length (subForest tree)) $ rootLabel tree

main :: IO() 
main = do
    input <- map read . words <$> readFile "input/day8.txt"
    let tree = head . fst $ parseTree ([], input)
    print "I didn't write this, I took it from reddit to understand how to build/use trees in haskell"
    print $ part1 tree
    print $ part2 tree
