constructor:: a -> [a] -> [a]
constructor y [] = []
constructor y (x:xs) = y : x : xs

cardinal :: [Int] -> Int
cardinal [] = 0
cardinal (x:xs) = 1 + cardinal xs

indice0 :: [a] -> Int -> a
indice0 (x:xs) 0 = x
indice0 (x:xs) n = indice0 xs (n-1)

tomar :: [a] -> Int -> [a]
tomar (x:xs) 0 = []
tomar (x:xs) n = x : tomar xs (n-1)

tirar :: [a] -> Int -> [a]
tirar (x:xs) 1 = xs
tirar (x:xs) n = tirar xs (n-1)
 
cabeza :: [a] -> a
cabeza (x:xs) = x

cola :: [a] -> [a]
cola (x:xs) = xs

pegarfinal :: [a] -> a -> [a]
pegarfinal [] a = [a]
pegarfinal (x:xs) a = x : pegarfinal xs a

pegar :: a -> [a] -> [a]
pegar a [] = [a]
pegar a (x:xs) = pegar a (x:xs)

concatenar :: [a] -> [a] -> [a]
concatenar [] ys = ys
concatenar (x:xs) ys = x:(concatenar xs ys)
