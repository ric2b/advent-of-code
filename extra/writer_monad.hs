-- newtype Writer w a = Writer { runWriter :: (a, w) }

-- instance (Monoid w) => Monad (Writer w) where
--     return x = Writer (x, mempty)
--     (Writer (x, v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')
import Control.Monad.Writer

main = do 
    print $ (6, 2)
    -- print $ runWriter (return 3 :: Writer String Int)
    print $ f $ (return 3 :: Writer [String] Int)
    return ()

f w = case runWriter w of
    (x, y) -> x