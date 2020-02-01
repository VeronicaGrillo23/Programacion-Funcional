----PARA USAR HASKELL---------------------------------
--EM EL CMD -------------------------------------------
----- > D: --------------------------------------------
----- > D:/dir ----------------------------------------
----- > ghci --------------------------
----- > :l practico4.hs ------------------------------
---dir es D:\Mis Documentos\FAMAF 2019\2019\AULA VIRTUAL\pract\23SEGUNDOPARCIAL\PRACTICO 4---
------------------PRACTICO 4--------------------------
------------------FIGURA------------------------------
data Color = Rojo | Amarillo | Azul | Verde
			deriving (Show, Eq)
data Forma = Triangulo | Cuadrado | Rombo | Circulo
			deriving (Show, Eq)
type Figura = (Forma, Color, Int)
------------------EJERCICIO 1--------------------------
rojo, azul, amarillo, verde :: Figura -> Bool
rojo (f, c, t) = (c == Rojo)
azul (f, c, t) = (c == Azul)
amarillo (f, c, t) = (c == Amarillo)
verde (f, c, t) = (c == Verde)

circulo, rombo, cuadrado, triangulo :: Figura -> Bool
circulo (f, c, t) = (f == Circulo)
rombo (f, c, t) = (f == Rombo)
cuadrado (f, c, t) = (f == Cuadrado)
triangulo (f, c, t) = (f == Triangulo)
------------------EJERCICIO 2--------------------------
tam :: Figura -> Int
tam (f, c, t) = t
------------------EJERCICIO 3--------------------------
------------------EJERCICIO 3a-------------------------
---------Todas las Figuuras de xs son rojas------------
--------- (A x : x Єl xs : rojo x)---------------------
------------------EJERCICIO 4a-------------------------
------------------Ejemplo------------------------------
----- xs = [( , , ), ( , , ), ( , , )]-----------------
----- xs' = [( , , ), ( , , ), ( , , )]----------------
------------------EJERCICIO 5a-------------------------
todasRojas, todasRojasb :: [Figura] -> Bool
todasRojas [ ] = True
todasRojas (x : xs) = rojo x && todasRojas xs

todasRojasb [ ] = True
todasRojasb (x : xs)| rojo x = todasRojasb xs
		    |otherwise = False
------------------EJERCICIO 5b--------------------------
todasMenores5, todasMenores5b :: [Figura] -> Bool
todasMenores5 [] = True
todasMenores5 (x : xs) = tam x < 5 && todasMenores5 xs

todasMenores5b [] = True
todasMenores5b (x : xs)| tam x < 5 = todasMenores5 xs
		       |otherwise = False
------------------EJERCICIO 5c--------------------------
todosTriaRojosb :: [Figura] -> Bool
todosTriaRojosb [] = True
todosTriaRojosb (x : xs)| triangulo x && rojo x = todosTriaRojosb xs
		        |otherwise = False

------------------EJERCICIO 5d--------------------------
--cincoD :: [Figura] -> Bool
--cincoD [] = False
--cincoD (x:xs)| not(cuadrado x && verde x) = cincoD xs 
--	     |otherwise = True

------------------EJERCICIO 5e--------------------------
circuloAzulMenor10 :: [Figura] -> Bool
circuloAzulMenor10 [] = False
circuloAzulMenor10 (x : xs) = circulo x && (azul x && tam x<10) || circuloAzulMenor10 xs
------------------EJERCICIO 5f--------------------------
noTriaAzul :: [Figura] -> Bool
noTriaAzul [] = True
noTriaAzul (x : xs) | triangulo x = (not (azul x)) && noTriaAzul xs
		    | otherwise = True
------------------EJERCICIO 5g--------------------------
nohayCAV :: [Figura] -> Bool
nohayCAV [] = False
nohayCAV (x : xs) = (circulo x == (not (amarillo x) == not (verde x))) || nohayCAV xs
------------------EJERCICIO 5h--------------------------
circuloMenor5 :: [Figura] -> Bool
circuloMenor5 [] = False
circuloMenor5 (x :xs) = (circulo x && tam x<5) || circuloMenor5 xs
------------------EJERCICIO 5i--------------------------
--hayCiRCuaR :: [Figura] -> Bool
--hayCiRCuaR [] = False
--hayCiRCuaR (x :xs) = (not(circulo x && rojo x) || (cuadrado x && rojo x)) || hayCiRCuaR xs






