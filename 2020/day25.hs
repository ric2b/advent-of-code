main = do
    (publicKeyA:publicKeyB:[]) <- map read <$> lines <$> readFile "input/day25.txt"
    print $ transform publicKeyB (breakLoopNumber publicKeyA (1, 0)) -- 6011069

breakLoopNumber publicKey (value, loop) = 
    if publicKey == value then loop else breakLoopNumber publicKey (step 7 value, loop+1)

transform subjectNumber loops = iterate (step subjectNumber) 1 !! loops

step subjectNumber value = value * subjectNumber `mod` 20201227
