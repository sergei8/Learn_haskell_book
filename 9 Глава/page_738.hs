import System.IO
import Data.Char (digitToInt)

main = do

  -- читать файл как строку
  fString <- readFile "todo.txt"
  -- разбить строкуи на массив
  let aString = splitString fString
  -- создать массив пар [(n,s)] n - номер строки, s - строка
  let numLines = zip [1..] aString
  -- вывести строки с номерами на экран
  putStrLn "Your tasks:"
  putStrLn "----------"
  mapM_ putStrLn [ (show $ fst x) ++ " " ++ (cutString $ snd x) ++ "..." | x <- numLines]
  -- ввести номер строки для удаления
  putStr "type number: "
  chr <- getChar
  let numToDelete = digitToInt chr
  -- удалить из массива элемент с введенным номером
  let numLines' = filter (\(n,v) -> n /= numToDelete) numLines
  -- вытащить из массива контент для записи в файл
  let fString' = foldr (\(n,v) l -> v ++ "\n" ++ l) "" numLines'
  -- перезаписаь новый файл
  writeFile "todo'.txt" fString'
  putStrLn "\nready"

  return ()

splitString :: String -> [String]     -- aka `lines` !
splitString str =
  map reverse
    $ reverse
    $ foldl (\arr@(x:xs) c -> if c /= '\n'
                                then [[c] ++ x] ++ xs
                                else [""] ++ arr
            ) [""] str

cutString :: String -> String
cutString =
  foldl (\s c -> if length s >= 10 then s else s ++ [c]) ""
