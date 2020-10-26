import Control.Monad.State

-- data State s a = State { runState :: s -> (s, a) }


reverseWithCount :: [a] -> State Int [a]
reverseWithCount list = State (\s ->
  (s + 1, reverse list))

appendReversedWithCount :: [a] -> [a] -> State Int [a]
appendReversedWithCount list1 list2 =
  reverseWithCount list1 >>= (\revList1 ->
    reverseWithCount list2 >>= (\revList2 ->
      state (\s -> (s + 1, revList1 ++ revList2))))
