> module HaskellLab3HW3
>	where

Problem 1

> allTrue = foldr (\y z-> if (z == False) then False else y) True

allTrue [True, True, True, True, True]
returns True
allTrue [True, True, False, False, True]
returns false

Problem 2

> noneTrue = foldl (\x y-> if y == True then False else x) True

Problem 3a

> flattenT xs = foldl (\x (y,z) -> x ++ (y:z:[])) [] xs

Problem 3b

> flattenR xs = foldl (\x (y,z) -> z:y:x) [] xs

Problem 4a

> prodLtoR xs = ltorHelp 1 xs

> ltorHelp acc [] = []
> ltorHelp acc (x:xs) = [acc*x] ++ ltorHelp (acc*x) xs

Problem 4b

> prodLtoRHOF  = (.) reverse (foldl op [] )
>  where
>   op [] y = [y]
>   op (x:xs) y = (x*y):x:xs

Problem 5

> prodRtoLHOF  = (foldl op [] )
>  where
>   op [] y = [y]
>   op x y = (map(y*) x) ++ [y]

Problem 6

> replicate'' [] = []
> replicate'' x = reverse (foldl (++) [] (foldl (\y z -> (replicate z z):y) [] x))

Problem 7

> hamming = [x | x <- [1 ..], onlyP235 (factors x)]

> factors x = [y | y<- [1 .. x], mod x y == 0]

> onlyP235 [] = True
> onlyP235 (x:xs)
>   | x == 2                        = True && (onlyP235 xs)
>   | x == 3                        = True && (onlyP235 xs)
>   | x == 5                        = True && (onlyP235 xs)
>   | not(length (factors x) == 2)  = True && (onlyP235 xs)
>   | otherwise                     = False

Problem 8

> sumHarmonic x = sumH [1 .. x]
> sumH xs = foldl (\y z -> (1/z)+y) 0 xs

Problem 9

> sumSqr x = foldl (\y z -> (z^2)+y) 0 (digits x)

> isHappy n
>   | n == 1    = True
>   | n == 4    = False
>   | otherwise = isHappy (sumSqr n)

> digits n
>  | n < 10    = [n]
>  | otherwise = (mod n 10) : digits (div n 10)

> happySeq = filter isHappy [1 ..]

> sqrSeq n
>  | n == 1 || n == 4 = [n]
>  | otherwise        = n : sqrSeq (sumSqr n)
