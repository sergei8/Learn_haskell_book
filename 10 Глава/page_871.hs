-- | вычисление ОПЗ вида: x y - z + ..
import Data.List.Split (endBy)
import Data.Foldable
import Data.Char (isDigit)

-- data Res = Either String Float

main = do

  rowStr <- getLine

  -- семантический анализ строки
  -- let opz = analyseOpz $ filter (/="") $ endBy " " rowStr
  let opz = analyseOpz $ words rowStr
  case opz of
    Nothing -> putStrLn "Ошибка в строке"
    Just x  ->
      do
        let res = calcOpz $ x
        putStrLn $ "результат: " ++ show (snd res)

-- выполнение вычисления ОПЗ с записью результата
-- в виде пары (стек, результат)
calcOpz :: [String] -> ([String], Float)
calcOpz =
  foldl (\(stack, res) item ->
            if isOperation item == True
              then do

                -- выбрать из стека 2 числа,
                let (x, y) = get2Number stack

                -- выплнить опреацию `item`
                let res' = doOperation item x y

                -- затолкать результат в стек
                let stack' = [(show res')] ++ stack

                -- возвратить накопленный результат
                (stack', res')

              else
                -- затолкать число `x` в стек
                ([item] ++ stack, res)

        ) ([],0)


-- проверка корректности ОПЗ (не полностью)
analyseOpz :: [String] -> Maybe [String]
analyseOpz []       = Nothing
analyseOpz (x:[])   = Nothing
analyseOpz (x:y:[]) = Nothing
analyseOpz str@(x:y:xs) | (isNumber x) && (isNumber y) /= True = Nothing
                        | otherwise = Just str where
                            isNumber :: String -> Bool
                            isNumber z = all isDigit z

-- выполнение операции над двумя числами
doOperation :: String -> Float -> Float -> Float
doOperation opId x y
  | opId == "+" =  y + x
  | opId == "-" =  y - x
  | opId == "*" =  y * x
  | opId == "/" =  y / x

-- является ли строка операцией
isOperation :: String -> Bool
isOperation x   | x == "+"  = True
                | x == "-"  = True
                | x == "*"  = True
                | x == "/"  = True
                | otherwise = False

-- получить первую пару числе из стека
get2Number :: [String] -> (Float, Float)
get2Number (x : y : _) = (read x :: Float, read y :: Float)
