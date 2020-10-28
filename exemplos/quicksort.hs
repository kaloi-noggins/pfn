quicksort [] = []
quicksort (x:xs) = quicksort smol ++ [x] ++ quicksort bong
    where 
        smol = filter (<=x) xs
        bong = filter (>x) xs
