-- высисляет кратчайший путь на ориетнированном графе без петель (DAG)
import Data.List (splitAt, elemIndex)

main = do

  let pathArray = getPathArray
  -- проверить, является ли граф связным

  let shortPath = calcShortPath pathArray [] (0,0)  -- начальные значения

  putStrLn $ "результат: " ++ show shortPath ++ ", длина = " ++ show (sum shortPath)

-- | вычисляет рекурсивно и накапливает кратчайшие пути между узлами графа
--   pathArray - матрица графа с путями
--   resPath - список кратчайших расстояний - нвкапливается при рекурсии
--             является результатом
--   (i,j)  - координаты (строка/столбец) найденного минимального расстояния
calcShortPath :: [[Int]] -> [Int] -> (Int, Int) -> [Int]
calcShortPath pathArray resPath (i, j)
   | i == endPoint = resPath  -- условие прекращения рекурсии - индекс последней точки = длине массива (-1)
   | otherwise = do
     -- найти минимальный эл-т в строке
     let minPath = findMinPath (pathArray !! i) -- i-я строка
     -- модифицировать матрицу путей: забить 999999 зеркальное минимальное
     -- значение i-индекс которого = j-индексу из результата minPath
     let pathArray' = modifyPathArray pathArray (snd minPath, i)
     -- рекурсия с новыми значениями
     calcShortPath pathArray' (resPath ++ [fst minPath]) (snd minPath, i)
   where
     endPoint = (length pathArray) - 1

-- | ищет минимальный эл-т и его индекс в списке
--  pathList - строка матрицы путей
 -- возвражает пару minPath - мин.эл-т, minIdx - его индекс
findMinPath :: [Int] -> (Int, Int)
findMinPath pathList =
  (minPath, minIdx) where
    minPath = minimum pathList
    minIdx =
        case findElem of
          (Just x) -> x
          Nothing  -> 0
        where
          findElem = elemIndex minPath pathList

-- | модифицирует матрицу путей
--  pathArray - матрица
--  (j,i) - координаты эл-та
modifyPathArray :: [[Int]] -> (Int, Int) -> [[Int]]
modifyPathArray pathArray (j, i) =
  do
    -- модифицируем элемент в j-строке
    let item = pathArray !! j
        (xs, ys) = splitAt i item
        newItem = xs ++ ((99999999::Int) : tail ys)
    -- всавляем новую строку в массив
    let (xxs, yys) = splitAt j pathArray
        newPathArray = xxs ++ ((newItem :: [Int]) : tail yys)

    newPathArray  -- возвращаем модифицированный массив

-- | проверка, является ли массив связанным графом
--   xs - исходный массив
-- isConnected :: [[Int]] -> Bool
-- isConnected xs = do
--   -- преобразовать эл-ты xs в пары (x,s), где x - эл-т, s - начальное состояние
--   let xs0 = etap0 xs
--   -- построить список пар индексов для массива
--   let idxs = getIdxs $ length xs0
-- -- 1-й этап алгорима http://algolist.ru/maths/graphs/linked.php
--   let xs1 = etap1 xs0
-- -- 2-й этап:
--
--   True


getIdxs :: Int -> [(Int,Int)]
getIdxs n = [(i, j) | i <- [0..n], j <- [0..n] ]

-- -- построить начальные маркеры (эл-т j)
-- etap0 :: [[Int]] -> [[(Int, Int)]]
-- etap0 xxs = map (\xs -> map (\x -> (x, 0)) xs) xxs
--
-- -- пометить 1-ю вершину 2-м маркером
-- etap1:: [[(Int, Int)]] -> [[(Int, Int)]]
-- etap1 (xs:xxs) = ( (\ ((a,b):xs') -> (a,1) : xs')  xs) : xxs
--
-- etap2:: [[(Int, Int)]] -> [(Int, Int)]-> [[(Int, Int)]]
-- etap2 xs idxs =
--   foldl (\acc (i,j) -> [helper xs (i,j)] ++ [acc] ) [] idxs
--
-- helper xs (i,j) =
--   if snd el == 1
--     then (i,2)
--     else (i,j) where
--       el = xs !! i !! j
--
-- setMarker2 :: [[(Int, Int)]] -> (Int, Int)-> [(Int, Int)]
-- setMarker2 xs idx = undefined
--
-- setMarker3 :: (Int,Int) -> (Int,Int)
-- setMarker3 (a,b) = (a,2)
--
getPathArray :: [[Int]]
getPathArray =
  -- [
  --   [999, 50,  10,  999, 999, 999, 999, 999],   -- 0
  --   [50,  999, 30,  5,   999, 999, 999, 999],   -- 1
  --   [10,  30,  999, 999, 90,  999, 999, 999],   -- 2
  --   [999, 5,   999, 999, 20,  40,  999, 999],   -- 3
  --   [999, 999, 90,  20,  999, 999, 2,   999],   -- 4
  --   [999, 999, 999, 40,  999, 999, 25,  10 ],   -- 5
  --   [999, 999, 999, 999, 2,   25,  999, 8  ],   -- 6
  --   [999, 999, 999, 999, 999, 10,  8,   999]    -- 7
  -- ]
  --    0     1    2   3    4    5    6     7

    [
      [999,  1,   3,  999],
      [1,    999, 999, 2 ],
      [3,    999, 999, 4 ],
      [999,  2,   4  ,999]
    ]
