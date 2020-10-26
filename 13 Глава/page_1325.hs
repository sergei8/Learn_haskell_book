import System.Random

threeCoins :: Int -> (Bool, Bool, Bool) 
threeCoins init =
    (x, y, z) where
        (x, genX) = random $ mkStdGen init :: (Bool, StdGen)
        (y, genY) = random $ genX
        (z, _) = random $ genY
