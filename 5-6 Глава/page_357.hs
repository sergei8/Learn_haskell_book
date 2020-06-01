-- как много корней натуральных чисел нам потребуется,
-- чтобы их сумма превысила 1000

sqrtSumsGT1000 :: [Float] -> [Float]
sqrtSumsGT1000 xs = takeWhile (< 1000) $ scanl (\acc x -> sqrt x + acc ) 0 xs
