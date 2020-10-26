reverseWithCount :: Int -> [a] -> (Int, [a])
reverseWithCount funcCount list = (funcCount + 1, reverse list)


rev_3 :: Int -> [a] -> [a] -> [a] -> (Int, [a])
rev_3 n xs ys zs = 
    foldl builder (n, []) [xs, ys, zs]

builder :: (Int, [a]) -> [a] -> (Int, [a])
builder (n, list) nextList = 
  (fst (reverseWithCount n nextList), (snd $ reverseWithCount n list) ++ (snd $ reverseWithCount n nextList))



append3ReversedWithCount :: Int -> [a] -> [a] -> [a] -> (Int, [a])
append3ReversedWithCount funcCount list1 list2 list3 =
  let (funcCount', revList1) = reverseWithCount funcCount list1
      (funcCount'', revList2) = reverseWithCount funcCount' list2
      (funcCount''', revList3) = reverseWithCount funcCount'' list3
  in (funcCount''' + 1, revList1 ++ revList2 ++ revList3)

newtype State s a = State { runState :: s -> (s, a) } {- deriving (Show) -}


-- data State s a = State { runState :: s -> (s, a) }
-- data State s a = State (s,a) deriving (Show)


reverseWithCount' :: [a] -> State Int [a]
reverseWithCount' list = State (\s ->
  (s + 1, reverse list))

appendReversedWithCount' :: [a] -> [a] -> State Int [a]
appendReversedWithCount' list1 list2 =
  reverseWithCount' list1 >>= (\revList1 ->
    reverseWithCount' list2 >>= (\revList2 ->
      State (\s -> (s + 1, revList1 ++ revList2))))
