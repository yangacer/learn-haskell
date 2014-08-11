delete_all_lc :: (Eq a) => a -> ([a] -> [a])
delete_all_lc item list = [ i | i <- list, i /= item]

