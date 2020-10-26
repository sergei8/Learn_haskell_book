import Control.Monad

addSome :: Float -> Float 
addSome = do
    a <- (*2)
    b <- (+3)
    c <- (/3)
    return (a + b - c)

addSome' :: Int -> Int
addSome' x =
    x >>= (*2)