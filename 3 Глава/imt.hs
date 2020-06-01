newtype ImtGroup = ImtGroup [(Float, String)]
-- ImtGroup = [    (skynny, "Тощий")
--              ,  (normal, "Норма")
--              ,  (fat, "Полный")
--              ,  (superfat, "Жиртрест")
--             ]
skynny  = 18.5
normal  = 25.0
fat     = 30.0

imt :: Float -> Float -> IO()
imt w h | im < skynny                 = putStrLn "Тощий"
        | im >= skynny && im < normal = putStrLn "Норма"
        | im >= normal && im < fat    = putStrLn "Полный"
        | otherwise                   = putStrLn "Жиртрест"
        where
          im = w / h^2
          -- (skynny, normal, fat)  = (18.5, 25, 30)

          -- ...
