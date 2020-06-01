import qualified Data.Map as M

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String
type LockerNumber = Int
type FindError = String
type LockerMap = M.Map Int (LockerState, Code)

lockerList = [
                (1,(Taken,"111"))
              , (2,(Free,"222"))
              , (3,(Free,"333"))
             ]
lockerLookup :: LockerNumber -> Either FindError Code
lockerLookup number | lockerNumber == Nothing = Left "no such number"
                    | otherwise = if isTaken lockerNumber == True
                                    then Left "locker is busy "
                                    else Right $ getCode lockerNumber
                      where
                        lockerMap = M.fromList lockerList :: LockerMap
                        lockerNumber = M.lookup number lockerMap

isTaken :: Maybe (LockerState, Code) -> Bool
isTaken (Just (st, _)) = st == Taken

getCode :: Maybe (LockerState, Code) -> Code
getCode (Just (_, c)) = c
