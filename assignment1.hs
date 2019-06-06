let mylast a = last a

let myreverse a = reverse a

let isPalindrome a = if a == reverse a then True
                     else False

let compress xs = [ xs!!x | x <-[0..(length(xs)-1)] , if x == length(xs)-1 then True else xs!!x /= xs!!(x+1) ]

let dupli xs = [ xs!!(div x 2) | x<-[0..(2*length(xs) - 2 )] ]

let rotate a b = (drop b a) + (take b a)

let insertAt x xs n = [ if y == n-1 then x else if y < n-1 then xs!!y else xs!!(y-1) | y<-[0..length(xs)] ]

let combinations 0 = [[]]
    combinations n xs = [ xs!!index : x | index <- [0..(length(xs)-1)] , x <- combinations (n-1) (drop(index +1) xs) ]

let isPrime 1 = True
    isPrime 2 = False
    isPrime n = if length [ x | x <- [2 .. n-1] , n `mod` x == 0 ] == 0 then True else False


