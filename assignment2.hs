string2int x = read x + 0

isPrime 1 = True
isPrime 2 = False
isPrime n = if length [ x | x <- [2 .. n-1] , n `mod` x == 0 ] == 0 then True else False
infprime = [ x | x <- [2..] , isPrime x == True]

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
inffib = [ fib x | x <- [0..] ]

subsets [x] = [[],[x]]
subsets (x:xs) = subsets xs ++ [[x] ++ a | a <- (subsets xs) ]


         
length1 x y = [ a | a <- y , length a >= length x ]

isSubString x y 
 | (length y) == (length x) = if y == x then True else False
 | (length y > length x) = if (take (length x) y) == x then True else isSubString x (tail y)

substring x y = [ a | a <- (length1 x y), isSubString x a == True ] 




word2string n = if n < 1000
                  then num2word n
                else if n < 1000000
                  then num2word (n `div` 1000) ++ "thousand, " ++ word2string (n `mod` 1000)
                else num2word (n `div` 1000000) ++ "million, " ++ word2string (n `mod` 1000000)

num2word 0 = ""
num2word 1 = "one "
num2word 2 = "two "
num2word 3 = "three "
num2word 4 = "four "
num2word 5 = "five "
num2word 6 = "six "
num2word 7 = "seven "
num2word 8 = "eight "
num2word 9 = "nine "
num2word 10 = "ten "
num2word 11 = "eleven "
num2word 12 ="twelve "
num2word 13 ="thirteen "
num2word 14 ="fourteen "
num2word 15 ="fifteen "
num2word 16 ="sixteen "
num2word 17 ="seventeen "
num2word 18 ="eighteen "
num2word 19 ="nineteen "
num2word n = if n < 30
               then "twenty " ++ num2word (n-20)
             else if n < 40
               then "thirty " ++ num2word (n-30)
             else if n < 50
               then "fourty " ++ num2word (n-40)
             else if n < 60
               then "fifty " ++ num2word (n-50)
             else if n < 70
               then "sixty " ++ num2word (n-60)
             else if n < 80
               then "seventy " ++ num2word (n-70)
             else if n < 90
               then "eighty " ++ num2word (n-80)
             else if n < 100
               then "ninety " ++ num2word (n-90)
             else if n `mod` 100 == 0
               then num2word (n `div` 100) ++ "hundred "
             else num2word (n `div` 100) ++ "hundred and " ++ num2word (n - 100*(n `div` 100))
