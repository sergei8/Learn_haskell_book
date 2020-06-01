import System.IO
import Control.Exception
import System.Environment

main =
  do
    -- получить и разобрать параметр командной строки
    args <- getArgs
    case args of
      []    -> putStrLn "args string is empty"
      (x:_) -> do
        let fileName = x -- выделить имя файла
        -- читать файл и обработать ошибку чтения
        content <- catch (readFile fileName)
          (
            \e ->
              -- обработчик ошибки
              do
                let err = show (e :: IOException)
                putStrLn $ "Warning: Couldn't open " ++ fileName
                            ++ ": " ++ err
                return ""
          )
        -- вывести количество строк в файле
        putStrLn $ "File " ++ fileName ++ " has "
                    ++ (getLinesCount content) ++ " lines"
          where
            getLinesCount :: String -> String
            getLinesCount = show . length . lines
