import Data.List (nub, sort)
import Control.Monad.Writer
import Control.Monad.State

main = do 
    print $ moveKnight (6, 2)
    print $ sort $ nub $ in3 (6, 2)
    print $ sort $ nub $ inN 3 (6, 2)
    print $ runWriter (return 3 :: Writer String Int)
    -- print $ runWriter multWithLog
    -- print $ in3WithLog (writer ((6, 2), []))
    print $ runWriter $ (return (6, 2) :: Writer [KnightPos] KnightPos) --`applyLog` moveKnightWithLog
    print $ (return (writer ((6, 2), [(1,1)])) :: [Writer [KnightPos] KnightPos])
    print $ ([(writer ((6, 2), []))] :: [Writer [KnightPos] KnightPos])
    -- print $ runWriter (return (6, 2) :: Writer [KnightPos] KnightPos) `applyLog` moveKnightWithLog
    -- print $ moveKnight (6, 2) >>= map logMove . moveKnight
    -- print $ return (6, 2) >>= moveKnightWithLog >>= (>>= moveKnightWithLog)
    -- print $ return (6, 2) >>= moveKnightWithLog
    print $ runWriter multWithLog
    print $ moveKnight'' [(6, 2)]
    print $ runWriter $ liftedMoveKnight $ liftedMoveKnight $ liftedMoveKnight (return (6, 2) :: Writer [KnightPos] KnightPos)
    -- print $ logMove start <$> moveKnight start

    let x = (writer ((0, 0), []) :: Writer [KnightPos] KnightPos)
    let y = (return (writer ((0, 0), [])) :: [Writer [KnightPos] KnightPos])
    -- let z = (ListAndWriter [writer ((0, 0), [(0, 0)])] :: ListAndWriter [KnightPos] KnightPos)
    -- let y = (return (writer ((0, 0), [])) :: ListAndWriter [KnightPos] KnightPos)
    -- let x = (return ([writer ((0,0), [])]) :: ListAndWriter [KnightPos] KnightPos)
    return ()

type KnightPos = (Int,Int)

-- inNwithLog n start = iterate (>>= (moveKnight)) (return start) !! n

-- in3WithLog start = moveAndLog' [start] >>= moveAndLog' >>= moveAndLog'

canReachInN :: Int -> KnightPos -> KnightPos -> Bool  
canReachInN n start end = end `elem` inN n start

inN n start = iterate (>>= (moveKnight)) (return start) !! n

in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight

-- h :: Int -> Maybe Int
-- h = (\x -> return (x+1)) <=< (\x -> return (x*100)) 

-- moveKnightWithLog :: KnightPos -> [Writer [KnightPos] KnightPos]
-- moveKnightWithLog from = map (\to -> writer (to, [from])) possiblePositions
--     where possiblePositions = moveKnight from

-- moveKnight' :: KnightPos -> State [KnightPos] [KnightPos]
-- moveKnight' history = do
--     (currentPosition:previous) <- get
--     return $ map (\newPosition -> newPosition:currentPosition:previous) (moveKnight currentPosition)

-- logMove :: KnightPos -> Writer [KnightPos] KnightPos
-- logMove position = writer (position, [position])

-- moveKnightWithLogs :: State [KnightPos] [KnightPos]
-- moveKnightWithLogs = do
--     (position:history) <- get
--     push position
--     return (moveKnight (6, 2))

-- mk :: Writer [KnightPos] KnightPos -> [Writer [KnightPos] KnightPos]
-- mk (position, previous) = do
--     tell [position]

moveKnight'' :: [KnightPos] -> [[KnightPos]]
moveKnight'' history@((c,r):previous) = [(c',r'):history | (c', r') <- [
        (c+2,r-1),(c+2,r+1),
        (c-2,r-1),(c-2,r+1),
        (c+1,r-2),(c+1,r+2),
        (c-1,r-2),(c-1,r+2)
        ], c' `elem` [1..8] && r' `elem` [1..8]
    ]

moveKnight' :: KnightPos -> [KnightPos]
moveKnight' (c,r) = [(c',r') | (c', r') <- [
        (c+2,r-1),(c+2,r+1),
        (c-2,r-1),(c-2,r+1),
        (c+1,r-2),(c+1,r+2),
        (c-1,r-2),(c-1,r+2)
        ], c' `elem` [1..8] && r' `elem` [1..8]
    ]

-- liftedMoveKnight :: Writer [KnightPos] KnightPos -> [Writer [KnightPos] KnightPos]
-- liftedMoveKnight :: Writer [KnightPos] KnightPos -> Writer [KnightPos] KnightPos
liftedMoveKnight :: Writer [KnightPos] KnightPos -> Writer [KnightPos] KnightPos
liftedMoveKnight positionWithHistory = do
    currentPosition <- positionWithHistory
    tell [currentPosition]
    return currentPosition
    -- return (moveKnight currentPosition)

-- liftedMoveKnight' :: Writer [KnightPos] KnightPos -> [Writer [KnightPos] KnightPos]
-- liftedMoveKnight' positionWithHistory = do
--     currentPosition <- positionWithHistory
--     tell [currentPosition]
--     map return (moveKnight currentPosition)
-- moveAndLog' :: [KnightPos] -> [Writer [KnightPos] KnightPos]
-- moveAndLog' ps = concatMap moveAndLog ps

-- moveAndLog :: KnightPos -> [Writer [KnightPos] KnightPos]
-- moveAndLog p = map (logMove p) $ moveKnight p

combineLogs :: Writer [KnightPos] KnightPos -> Writer [KnightPos] KnightPos -> Writer [KnightPos] KnightPos
combineLogs move1 move2 = liftM2 (flip const) move1 move2

-- combineLogs :: [Writer [KnightPos] KnightPos] -> Writer [KnightPos] KnightPos
-- combineLogs (lastMove:[]) = lastMove
-- combineLogs (firstMove:rest) = liftM2 (flip const) firstMove (combineLogs rest)

logMove :: KnightPos -> Writer [KnightPos] KnightPos
logMove position = writer (position, [position])

moveKnight :: KnightPos -> [KnightPos]  
moveKnight (c,r) = do  
    (c',r') <- nub [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)  
               ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)  
               ]
    guard (c' `elem` [1..8] && r' `elem` [1..8])  
    return (c',r')

-- [(6,2)] >>= map logMove . moveKnight 

-- [mKnightPos] -> [mKnightPos]

-- [mKnightPos] -> mKnighPos >>= (knightPos -> [knightPos]) 

-- knightPos -> [knightPos]


-- applyLog :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)
-- applyLog (x,log) f = let (y,newLog) = f x in (y,log `mappend` newLog)  
  
logNumber :: Int -> Writer [String] Int  
logNumber x = writer (x, ["Got number: " ++ show x])  
  
multWithLog :: Writer [String] Int  
multWithLog = do  
    a <- logNumber 3  
    b <- logNumber 5  
    return (a*b) 

-- moveKnight :: KnightPos -> [KnightPos]
-- Writer [KnightPos] KnightPos -> [Writer [KnightPos] KnightPos]


-- logNumber :: Int -> Writer [String] Int
-- logNumber x = writer (x, ["Got number: " ++ show x])

-- multWithLog :: Writer [String] Int
-- multWithLog = do
--     a <- logNumber 3
--     b <- logNumber 5
--     return (a*b)

-- newtype Writer w a = Writer { runWriter :: (a, w) }

-- instance (Monoid w) => Monad (Writer w) where
--     return x = Writer (x, mempty)  
--     (Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')

-- guard :: (MonadPlus m) => Bool -> m ()  
-- guard True = return ()  
-- guard False = mzero

-- instance MonadPlus [] where
--     mzero = []
--     mplus = (++)

-- class Monad m => MonadPlus m where
--     mzero :: m a
--     mplus :: m a -> m a -> m a

-- class Monoid m where  
--     mempty :: m  
--     mappend :: m -> m -> m  
--     mconcat :: [m] -> m  
--     mconcat = foldr mappend mempty


-- newtype ListAndWriter a b = ListAndWriter { runListAndWriter :: [Writer b a] } deriving Show

newtype ListAndWriter a b = ListAndWriter { runListAndWriter :: [Writer b a] } deriving Show

instance Functor (ListAndWriter a) where  
    fmap f writers = ListAndWriter $ map (fmap f) $ runListAndWriter writers

newtype Prob a = Prob { getProb :: [(a,Rational)] } deriving Show  

instance Functor Prob where  
    fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x,p)) xs