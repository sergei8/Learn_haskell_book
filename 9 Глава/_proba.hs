import System.IO
import Control.Exception
import System.Environment

main = do
  let f = "aaa.txt"
  catch (readFile f)
      (\e -> do let err = show (e :: IOException)
                hPutStr stderr ("Warning: Couldn't open " ++ f ++ ": " ++ err)
                return "")
