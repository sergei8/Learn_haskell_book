-- каково первое натуральное число, сумма цифр которого равна 40?
import Data.Char (digitToInt)
import Data.List (find)

number40 :: Maybe Int
number40 = find (\x -> calcSum x == 40) [1..]

calcSum :: Int -> Int
calcSum x = foldl (\sum' x -> sum' + digitToInt x) 0 (show x)
