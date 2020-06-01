main = interact isPalindrom

isPalindrom :: String -> String
isPalindrom line | line == reverse line = line ++ ": palindrome"
                 | otherwise            = line ++ ": no palindrome"
