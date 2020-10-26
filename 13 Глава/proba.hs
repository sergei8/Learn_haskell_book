{-# LANGUAGE ConstraintKinds #-}

type Stringy a = (Read a, Show a)

foo :: Stringy a => a -> (String, String -> a)
foo x = (show x, read)