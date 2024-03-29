mylast a = last a

myreverse a = reverse a

isPalindrome a = if a == reverse a then True else False

compress xs = [ xs!!x | x <-[0..(length(xs)-1)] , if x == length(xs)-1 then True else xs!!x /= xs!!(x+1) ]

dupli xs = [ xs!!(div x 2) | x<-[0..(2*length(xs) - 2 )] ]

rotate a b = (drop b a) ++ (take b a)

insertAt x xs n = [ if y == n-1 then x else if y < n-1 then xs!!y else xs!!(y-1) | y <- [0..length(xs)] ] 


combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = [ xs!!index : x | index <- [0..(length(xs)-1)] , x <- combinations (n-1) (drop(index +1) xs) ]

isPrime 1 = True
isPrime 2 = False
isPrime n = if length [ x | x <- [2 .. n-1] , n `mod` x == 0 ] == 0 then True else False

