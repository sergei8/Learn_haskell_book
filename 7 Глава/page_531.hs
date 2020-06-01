import qualified Data.Map as M

type PhoneBook n p = M.Map n p
phoneBook = [("mike", "11111"),("bob", "22222")]

findPhoneNumber :: String -> String
findPhoneNumber name = case M.lookup name mpb of
                          Just p    -> p
                          otherwise -> "Not found"
                       where
                          mpb = M.fromList phoneBook
