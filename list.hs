import Data.List (sort)

-- i starts from 1
ith :: [a] -> Int -> a
ith [] n = error "Out of bound"
ith (x:_) 1 = x
ith (_:xs) n
  | n < 1 = error "Out of bound"
  | otherwise = ith xs (n-1)

-- get last ele of a list
-- desc
-- Time complexity, space complexity

-- last v1
-- O(n), O(n)
last' :: [a] -> a
last' [x] = x
last' ( _ : xs ) = last' xs

last'' :: [a] -> a
last'' [] = error "Empty list"
last'' x = x !! (length x -1)

-- get last but one ele
last_but_one :: [a] -> a
last_but_one [x,y] = x
last_but_one (_: xs) = last_but_one xs
delete_all_lc :: (Eq a) => a -> ([a] -> [a])
delete_all_lc item list = [ i | i <- list, i /= item]

-- reverse list
reverse' :: [a] -> [a]
reverse' list = reverse_aux list []
  where
    reverse_aux [] temp = temp
    reverse_aux (x:xs) temp = reverse_aux xs $ x:temp

-- 'abba' -> True, 'abbab' -> False
isPlaindrome :: (Eq a) => [a] -> Bool
isPlaindrome list = list == reverse' list

-- traverse nested list
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs))  = flatten x ++ flatten (List xs)

-- not in list
notIn :: (Eq a) => [a] -> a -> Bool
notIn [] ele  = True
notIn (x:xs) ele
  | ele == x = False
  | otherwise = notIn xs ele

-- uniquify list
unique' :: (Eq a) => [a] -> [a]
unique' list = unique_aux list []
  where
    unique_aux [] result = result
    unique_aux (x:xs) result
      | notIn result x = unique_aux xs (result ++ [x])
      | otherwise = unique_aux xs result

-- group' remain processed result
group' :: (Eq a) => [a] -> [[a]] -> [[a]]
group' [] result = result
group' (x:xs) [] = group' xs [[x]]
group' (x:xs) (y:ys) 
  | [] == y || x == (y !! 0) = group' xs [y ++[x]] ++ ys
  | otherwise = y:(group' (x:xs) ys)

-- collect' remain accumulated processed
collect' :: (Eq a) => [a] -> [a] -> [[a]] -> [[a]]
collect' [] [] result = result
collect' [] current result = result ++ [current]
collect' (x:xs) [] result = collect' xs [x] result
collect' (x:xs) current result
  | x == head current = collect' xs (x:current) result
  | otherwise = collect' (x:xs) [] (result ++ [current]) 

-- pack identical element in one list
pack' :: (Ord a) => [a] -> [[a]]
pack' list = collect' (sort list) [] []

-- RLEncode
rleEncode_aux :: (Eq a) => [a] -> (Maybe a, Int) -> [(a, Int)] -> [(a, Int)]
rleEncode_aux [] (Nothing, 0) result = result
rleEncode_aux [] (Just x, n) result = result ++ [(x, n)]
rleEncode_aux (x:xs) (Nothing, 0) result = rleEncode_aux xs (Just x, 1) result
rleEncode_aux (x:xs) (Just v, n) result
  | x == v = rleEncode_aux xs (Just v, n + 1) result
  | otherwise = rleEncode_aux (x:xs) (Nothing, 0) (result ++ [(v,n)])

rleEncode :: (Eq a) => [a] -> [(a, Int)]
rleEncode input = rleEncode_aux input (Nothing, 0) []

-- RLEncode with Code datatype
data Code a = Multiple a Int | Single a deriving(Show)

rleEncode2_aux :: (Eq a) => [a] -> (Maybe (Code a)) -> [Code a] -> [Code a]
rleEncode2_aux [] Nothing result = result
rleEncode2_aux [] (Just x) result = result ++ [x]
rleEncode2_aux (x:xs) Nothing result = rleEncode2_aux xs (Just $ Single x) result
rleEncode2_aux (x:xs) (Just (Single v)) result
  | x == v = rleEncode2_aux xs (Just $ Multiple v 2) result
  | otherwise = rleEncode2_aux (x:xs) Nothing (result ++ [Single v])
rleEncode2_aux (x:xs) (Just (Multiple v n)) result
  | x == v = rleEncode2_aux xs (Just $ Multiple v $ n + 1) result
  | otherwise = rleEncode2_aux (x:xs) Nothing (result ++ [Multiple v n])

rleEncode2 :: (Eq a) => [a] -> [Code a]
rleEncode2 input = rleEncode2_aux input Nothing []

-- duplicate
dupl :: [a] -> [a]
dupl input = foldl (\accu x -> accu ++ [x] ++ [x]) [] input

-- replicate n
repl :: [a] -> Int -> [a]
repl input n = foldl (aux n) [] input
  where 
    aux 0 accu x = accu
    aux n accu x = aux (n-1) (accu ++ [x]) x
