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
        -- читать и обработать
        bracketOnError
          (readFile fileName)
          (\e -> putStrLn "Error")
          (\content -> putStrLn ( "\nFile " ++ fileName ++ " has "
                       ++ (show $ length $ lines content)  ++ " lines\n")
          )
