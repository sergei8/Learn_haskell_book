tpl :: (Int, String) -> Float
tpl (a, b) =
    case (a, b) of
      -- () -> 0
      (a,
      let b = if isNumber b
                        then read (show b)
                        else read "0"
      ) -> 12.2
      -- ()

isNumber :: String -> Bool
isNumber (b:bx) = True



  -- if (elem b ['A'..'Я'])
  --             then
