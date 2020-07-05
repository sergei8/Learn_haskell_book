newtype CoolBool = CoolBool Bool deriving (Show)
-- data CoolBool = CoolBool Bool deriving (Show)

hello :: CoolBool -> String
hello (CoolBool _) = "привет"
