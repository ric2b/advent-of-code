import qualified Data.Map as Map
import Data.List (union)

import Text.ParserCombinators.Parsec

main = do
    input <- lines <$> readFile "input/day07.txt"
    let parsedRules = Map.fromList $ map ((\(Right result) -> result) . parse parseBagContainsRule "") input
    print $ length $ bagsThatCanContain (bagColorParents parsedRules) "shiny gold" -- 242
    print $ sum $ Map.elems $ bagsContained parsedRules "shiny gold" -- 176035

type BagColor = String
type ContainedBagColors = Map.Map BagColor Int

bagsThatCanContain :: Map.Map BagColor [BagColor] -> BagColor -> [BagColor]
bagsThatCanContain parentMap bagColor = union directParents indirectParents
    where directParents = parentMap Map.! bagColor
          indirectParents = concat $ map (bagsThatCanContain parentMap) directParents :: [BagColor]

bagColorParents :: Map.Map BagColor ContainedBagColors -> Map.Map BagColor [BagColor]
bagColorParents rules = Map.foldlWithKey updateChildren initialParentMap rules
    where initialParentMap = Map.map (\_ -> []) rules
          updateChildren prevParentMap parentColor containedColors = foldl updateWithNewParents prevParentMap (Map.keys containedColors)
            where updateWithNewParents parentMap childColor = Map.alter addParent childColor parentMap
                    where addParent Nothing = Just [parentColor]
                          addParent (Just prevParents) = Just (parentColor:prevParents)

bagsContained :: Map.Map BagColor ContainedBagColors -> BagColor -> Map.Map BagColor Int
bagsContained rules bagColor = Map.unionsWith (+) (directlyContainedColors:indirectlyContainedColors)
    where directlyContainedColors = rules Map.! bagColor
          indirectlyContainedColors = map amountAdjustedContainedColors (Map.toList directlyContainedColors) :: [Map.Map BagColor Int]
            where amountAdjustedContainedColors (containedColor, amount) = Map.map (amount *) (bagsContained rules containedColor)

parseBagContainsRule :: Parser (BagColor, ContainedBagColors)
parseBagContainsRule = do
    bagColor <- manyTill anyChar (try (string " bags contain "))
    containedBags <- [] <$ string "no other bags." <|> (many parseContainedBag)
    eof
    return (bagColor, (Map.fromList containedBags))

parseContainedBag :: Parser (BagColor, Int)
parseContainedBag = do
    containedBagAmount <- read <$> many1 digit
    char ' '
    containedBagColor <- manyTill anyChar ((try (string " bags") <|> try (string " bag")) >> (try (string ".") <|> try (string ", ")))
    return (containedBagColor, containedBagAmount::Int)
