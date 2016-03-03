quicksort :: (Ord a) => [a] -> [a]

quicksort [] = []
quicksort (x:rest) = (quicksort (filter (<x) rest)) ++
                   [x] ++
                   (quicksort (filter (>= x) rest ))
