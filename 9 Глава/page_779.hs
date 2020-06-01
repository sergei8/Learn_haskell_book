-- | случайный пользовательсуий тип

import System.Random

data R = True' | False'

class Show' a where
  show' :: a -> String

instance Show' R where
  show' True' = "True"
  show' False' = "False"

instance Random R where
  random r = (True', r)


main = do
  let rnd = random $ mkStdGen 10 :: (R, StdGen)
  putStrLn $ show' $ fst $ rnd
  return ()
