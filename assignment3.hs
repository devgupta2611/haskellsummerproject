sumsq :: (Num a) => a -> a
sumsq n = foldr (\x acc -> acc + x^2) 0 [1..n]

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr(\x acc -> f x : acc) [] xs


filter' f xs = foldr(\x acc -> if f x then x : acc else acc) [] xs


reverse' xs = foldr(\x acc -> acc ++ [x]) [] xs


reverse' xs = foldl(\acc x -> x : acc) [] xs


remdups xs = foldr(\x acc -> if x == acc !! 0 then acc else [x]++acc) [last xs] xs


inits xs = foldr(\x acc -> [] : map (x:) acc) [[]] xs
