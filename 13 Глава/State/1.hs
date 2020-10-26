-- Reverse a list, and increase a count of function calls
reverseWithCount :: Int -> [a] -> (Int, [a])
reverseWithCount funcCount list =
  (funcCount + 1, reverse list)

appendReversedWithCount :: Int -> [a] -> [a] -> (Int, [a])
appendReversedWithCount funcCount list1 list2 =
  let (funcCount', revList1) = reverseWithCount funcCount list1
      (funcCount'', revList2) = reverseWithCount funcCount' list2
  in (funcCount'' + 1, revList1 ++ revList2)


  