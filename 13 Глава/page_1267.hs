-- data Band a = Band {getIs :: fst a , getMsg :: snd a}
-- newtype Band = Band (Bool, String) deriving (Show)
newtype Band = Band {getBand :: (Int, String)} deriving (Show)

isBand :: Int -> Band
isBand x = Band (x, getMsg x) where
  getMsg :: Int -> String
  getMsg x | x < 3          = "small band "
           | x >=3 && x < 7 = "medium band "
           | otherwise      = "big band "

applyLog :: Band -> (Int -> Band) -> Band
applyLog (Band (x, msg)) f = Band (x, msg ++ msg') where
  Band (x', msg') = f x
