main = do
  return $ helper "aaa"
  return ()
  -- putStrLn "ccc"
  return "last!"

helper :: String -> IO String
helper s = s >> return s
