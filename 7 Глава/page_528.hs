type PhoneBook = [(Name, PhoneNumber)]
type Name = String
type PhoneNumber = String

phoneBook = [("mike", "11111"),("bob", "22222")] :: PhoneBook

findPhoneNumber :: Name -> PhoneNumber
findPhoneNumber n = case helper n phoneBook of
  Just phoneNumber -> phoneNumber
  otherwise        -> "Not found"

helper :: Name -> PhoneBook -> Maybe PhoneNumber
helper _ []   = Nothing
helper n (x:xs) = if n == fst x
                           then Just (snd x)
                           else helper n xs
