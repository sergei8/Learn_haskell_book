import  Control.Exception
check x y = catch (x `div` y `seq` return False)
                    (\_ -> return True)
