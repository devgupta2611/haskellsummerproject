let string2int x = read x + 0

let isPrime 1 = True
    isPrime 2 = False
    isPrime n = if length [ x | x <- [2 .. n-1] , n `mod` x == 0 ] == 0 then True else False
let infprime = [ x | x <- [2..] , isPrime x == True]


