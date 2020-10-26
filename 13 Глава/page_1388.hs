import Control.Monad.Writer

keepSmall :: Int -> Writer [String] Bool
keepSmall x
    | x < 4  = do
        tell ["Save " ++ show x]
        return True
    | otherwise = do
        tell [show x ++ " too big, reject"]
        return False
