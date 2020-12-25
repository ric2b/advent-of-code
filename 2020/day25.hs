main = do
    (publicKeyA:publicKeyB:[]) <- map read <$> lines <$> readFile "input/day25.txt"
    print $ transform publicKeyB (breakLoopNumber publicKeyA) -- 6011069

breakLoopNumber publicKey = snd $ until ((== publicKey) . fst) (\(value, i) -> (step 7 value, i+1)) (1, 0)

transform subjectNumber loops = iterate (step subjectNumber) 1 !! loops

step subjectNumber value = value * subjectNumber `mod` 20201227
