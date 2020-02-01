sum0 :: [Int] -> Int
sum0 [] = 0
sum0 (x:xs) = x + sum xs

duplica2 :: [Int]-> [Int]
duplica2 [] = []
duplica2 (x:xs) = (2*x) : duplica2 xs

sumar1 :: [Int]-> [Int]
sumar1 [] = []
sumar1 (x:xs) = (x+1) : sumar1 xs



