import System.Environment
import System.Random

data Coin = Heads | Tails deriving (Eq, Show)

-- class ShowCoin a where
--   show' :: [a] -> String
--
-- instance ShowCoin Coin  where
--     show' xs =
--       show $ foldr (\x s -> getCoinStr x : s ) [] xs where
--         getCoinStr x | x == Heads = "Heads"
--                      | otherwise  = "Tails"

main = do

  -- получить количество подбрасываний
  args <- getArgs

  let numFlips = getNumFlips args
  if numFlips == Nothing
    then putStrLn "no flips number"
    else putStrLn $ show $ generateFlips $ (\(Just x ) -> x) numFlips

generateFlips :: Int -> [Coin]
generateFlips 0 = []
generateFlips n =
  ( if rnd <= 0.5 then Heads else Tails ) : generateFlips (n-1) where
      (rnd,_) = random (mkStdGen n) :: (Float, StdGen)

-- | выбирает 1-й параметр в Maybe
getNumFlips :: [String] -> Maybe Int
getNumFlips [] = Nothing
getNumFlips (x:_) = Just ( read x :: Int)
