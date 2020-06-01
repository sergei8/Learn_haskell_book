import Control.Monad

main = do
  -- rs <- sequence [getChar, getChar]
  rs <- sequence [putStrLn "aa", putStrLn "dd"]
  return ()
