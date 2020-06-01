import System.IO
import Data.List.Split
-- import Control.Monad.IO.Class

main = do
  newToDo <- getLine     -- short todo#long todo

  let parsedNewToDo = parseToDo newToDo
  if parsedNewToDo == Nothing
    then putStrLn "Error in ToDo"
    else do
      let short = (\(Just x) -> fst x) parsedNewToDo
      let long  = (\(Just x) -> snd x) parsedNewToDo

      todoContent <- readFile "todo.txt"

      if  isToDoExist short (lines todoContent) == True
        then do
          putStrLn $ short ++ " already exist"
        else do
          appendFile "todo.txt" (newToDo ++ "\n")
          putStrLn $ short ++ " added"

parseToDo :: String -> Maybe (String, String)
parseToDo toDo =
  case splitOn "#" toDo of
    ["",_] -> Nothing
    [_]    -> Nothing
    [s,l]  -> Just (s,l)

isToDoExist :: String -> [String] -> Bool
isToDoExist toId toDos =
  foldl (\res toDo ->
          do
            if fmap (fst) (parseToDo toDo) == Just toId
              then res || True
              else res || False
        ) False toDos
