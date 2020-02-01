f :: Int -> Int
f x = 5 * x

duplica :: Int -> Int
duplica a = a + a

por2 :: Int -> Int
por2 y = 2 * y

multiplicar :: Int -> Int -> Int
multiplicar zz tt = zz * tt

promedio :: Float -> Float -> Float
promedio x y = (x + y)  / 2

g :: Int -> Int
g y = 8 * y

h :: Int -> Int -> Int
h z w = z+w

j :: Int -> Bool
j x = x<=0

signo :: Int -> Int
signo x | x>0 = 1
	| x<0 = (-1)
	| x==0 = 0

entre0y9 :: Int -> Bool
entre0y9 x| (0<x && x<9) = True
	  | otherwise = False

rangoPrecio :: Int -> String
rangoPrecio x | x<0 = "esto no puede ser"
	      | x<2000 = "muy barato"
	      | (2000<=x && x<=5000) = "hay que verlo bien"
	      | x>5000 = "muy caro"

absoluto :: Int -> Int
absoluto x | x<0 = (-x)
	   | x>=0 = x

esMultiplo2 :: Int -> Bool
esMultiplo2 n | mod n 2 == 0 = True
	      | otherwise = False

esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y | (mod x y == 0) = True
		 | otherwise = False

esBisiesto :: Int -> Bool
esBisiesto x | (mod x 400 ==0) || (mod x 4 == 0) && not(mod x 100 == 0) = True
	     | otherwise = False

dispersion :: Int -> Int -> Int -> Int
dispersion x y z = (max(max x y) z) - (min(min x y)z)

cToF :: Float -> Float
cToF x = (x * 1.8) + 32

fToC :: Float -> Float
fToC x = (x-32)/1.8

haceFrioF :: Float -> Bool
haceFrioF x = ((x - 32)/ 1.8) <= 8

sumaPares :: (Int, Int) -> (Int, Int) -> (Int, Int)
sumaPares (a,b) (c,d) = (a+c, b+d)

segundo3 :: (Int, Int, Int) -> Int
segundo3 (x, y, z) = y

ordena :: (Int, Int) -> (Int, Int)
ordena (x, y) = ((min x y),(max x y))

