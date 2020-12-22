import qualified Data.Set as S

main = do
    rawInput <- lines <$> readFile "input/day22.txt"
    let (name1:raw_deck1, _sep:name2:raw_deck2) = break (=="") rawInput
    let deck1 = map read raw_deck1
    let deck2 = map read raw_deck2

    print $ score $ playCombatUntilWin (deck1, deck2) -- 31957
    print $ score $ playRecursiveCombatUntilWin S.empty (deck1, deck2) -- 33212

score decks = case decks of 
    (winnerDeck, []) -> deckScore winnerDeck
    ([], winnerDeck) -> deckScore winnerDeck
    where deckScore = sum . zipWith (*) [1..] . reverse

playRecursiveCombatUntilWin :: S.Set ([Int], [Int]) -> ([Int], [Int]) -> ([Int], [Int])
playRecursiveCombatUntilWin _ (deck1, []) = (deck1, [])
playRecursiveCombatUntilWin _ ([], deck2) = ([], deck2)
playRecursiveCombatUntilWin history (deck1@(drawn1:rest1), deck2@(drawn2:rest2))
    | (deck1, deck2) `S.member` history = (deck1, [])
    | otherwise = playRecursiveCombatUntilWin newHistory (playRecursiveCombatRound (deck1, deck2))
    where newHistory = (deck1, deck2) `S.insert` history

playRecursiveCombatRound :: ([Int], [Int]) -> ([Int], [Int])
playRecursiveCombatRound (deck1@(drawn1:rest1), deck2@(drawn2:rest2)) = (newDeck1, newDeck2)
    where newDeck1 = tail deck1 ++ if player1Wins then [drawn1, drawn2] else []
          newDeck2 = tail deck2 ++ if player1Wins then [] else [drawn2, drawn1]
          player1Wins = (ableToRecurse && player1WinsSubgame) || ((not ableToRecurse) && (drawn1 > drawn2))
          player1WinsSubgame = null . snd $ playRecursiveCombatUntilWin S.empty (recurseDeck1, recurseDeck2)
          recurseDeck1 = take drawn1 rest1
          recurseDeck2 = take drawn2 rest2
          ableToRecurse = (length deck1 > drawn1) && (length deck2 > drawn2)

playCombatUntilWin :: ([Int], [Int]) -> ([Int], [Int])
playCombatUntilWin (deck1, []) = (deck1, [])
playCombatUntilWin ([], deck2) = ([], deck2)
playCombatUntilWin decks = playCombatUntilWin (playCombatRound decks)

playCombatRound :: ([Int], [Int]) -> ([Int], [Int])
playCombatRound (deck1@(drawn1:rest1), deck2@(drawn2:rest2)) = (newDeck1, newDeck2)
    where newDeck1 = rest1 ++ if player1wins then [drawn1, drawn2] else []
          newDeck2 = rest2 ++ if player1wins then [] else [drawn2, drawn1]
          player1wins = drawn1 > drawn2
