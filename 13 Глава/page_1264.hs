-- data Band a = Band {getIs :: fst a , getMsg :: snd a}
-- newtype Band = Band (Bool, String) deriving (Show)
newtype Band = Band {getBand :: (Bool, String)} deriving (Show)

isBand :: Int -> Band
isBand x = Band (x > 5, "check QTY")
