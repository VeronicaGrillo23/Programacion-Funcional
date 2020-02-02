-- / ///////////////////////////////////////////////////////////////////////////
-- / //////            PROYECTO 1                                         //////
-- / ///////////////////////////////////////////////////////////////////////////
-- ////VERONICA ELIZABETH GRILLO 
-- ////GABRIELA BONO
-- /////////////////////////////////////////////////////////////////////////////
--------NOTAS APARTE-DIGITALZACION DEL PROYECTO 1-------------------------------

-- AYED1 LABORATORIO 30 DE 14 A 18

-- CLASE1 01- 12 DE AGOSTO ------------------------------------------->AUSENTE
-- CLASE2 04- 19 DE AGOSTO ------------------------------------------->FERIADO
-- CLASE/ 07- 26 DE AGOSTO ULTIMA CONSULTA --------------------------->CONMSULTAR
-- / //////////////////----DEFINICION DE FUNCIONES POR COMPOSICION??????????????
-- / //////////////////----CUAL PRELUDIO????????????????????????????????????????
-- CLASE3 10- 02 DE SEPTIEMBRE EXAMEN ORAL DE PROYECTO 1 LABORATORIO-->?
-- * USA GHCI CON EL FLAG -WALL. ???????????????????????????????????????????????
--   SE VERIFICARA Q NO HAYA WARMINGS AL CARGAR EL ARCHIVO??????????????????????

-- PROYECTO 1 

-- FUNCIONES, TIPOS, CLASES Y ALTO ORDEN:

-- 1.OBJETIVO

-- REVISAR LA PROGRAMACION DE FUNCIONES EN HASKELL
-- INTRODUCIR CONCEPTOS DE POLIMORFISMO Y FUNCIONES DE ALTO ORDEN

-- SE EVALUARA:

-- DEFINICION DE FUNCIONES RECURSIVAS USANDO CASO BASE Y CASO INDUCTIVO 
-- (A TRAVEZ DE ANALISIS POR CASOS O PATTERN MATCHING)
-- DEFINICION DE FUNCIONES POR COMPOSICION ?????????????????????????????????????
-- EL USO DE FUNCIONES PROVISTAS POR EL LENGUAJE (EN EL PRELUDIO- CUAL PRELUDIO???
-- PARA LA DEFINICION DE FUNCIONES POLIMORFICAS

-- EN ALGUNAS FUNCIONES NECESITAREMOS UTILIZAR:
-- DEFINICIONES LOCALES Y USO DE GUARDAS PARA ALTERNATIVAS BOOLEANAS

-- EN OTROS CASOS SE DEBERA UTILIZAR:
-- APLICACION PARCIAL DE FUNCIONES Y OPERADORES BINARIOS

-- CONSIDERACIONES:

-- * HACE TOD0 EL PROYECTO EN UN MISMO ARCHIVO
-- * USA GHCI CON EL FLAG -WALL. ????????????????
--   SE VERIFICARA Q NO HAYA WARMINGS AL CARGAR EL ARCHIVO?????????  
-- * COMENTA EL CODIGO INDICANDO A QUE EJERCICIO CORRESPONDEN LAS FUNCIONES 
-- EJEMPLO -- EJERCICIO N

-- COMANDOS:

-- para mi pc
-- >:D
-- >cd d:/dir
-- >ghci
-- >:l Proyecto1.hs

-- //////////////////////////////////////////////////////////////////////////////////

-- / ///////////////////////////////////////////////////////////////////////////
-- / //////            PROYECTO 1                                         //////
-- / ///////////////////////////////////////////////////////////////////////////
-- ////VERONICA ELIZABETH GRILLO 
-- ////GABRIELA BONO
-- /////////////////////////////////////////////////////////////////////////////
-- PROYECTO 1 
-- FUNCIONES, TIPOS, CLASES Y ALTO ORDEN:

-- 1. Programa las siguientes funciones:

-- a) esCero :: Int -> Bool, que verifica si un entero es igual a 0.
-- LIZ
esCero :: Int -> Bool
esCero x | x == 0 = True
         | x /= 0 = False
-- GABY
esCero' :: Int -> Bool
esCero' x = x == 0 
-- EN GHCI
-- >esCero 0
-- True
-- >esCero 10
-- False
-- >esCero (-1)
-- False

-- b) esPositivo :: Int -> Bool, que verifica si un entero es estrictamente mayor a 0.
-- LIZ
esPositivo :: Int -> Bool
esPositivo x | x > 0 = True
             | otherwise = False
--GABY
esPositivo' :: Int -> Bool
esPositivo' x |(x>0)=True
              |(x<=0)= False
-- EN GHCI
-- >esPositivo 1
-- True
-- >esPositivo 0
-- False
-- >esPositivo (-1)
-- False

-- c) esVocal :: Char -> Bool, que verifica si un caracter es una vocal en minuscula.
--LIZ
esVocal :: Char -> Bool
esVocal x | (x == 'a') || (x == 'e') || (x == 'i') || (x == 'o') || (x == 'u') = True
          | otherwise = False
--GABY
esVocal' :: Char -> Bool
esVocal' n= (n=='a')||(n=='e')||(n=='i')||(n=='o')||(n=='u')
-- EN GHCI
-- >esVocal 'a'
-- True
-- >esVocal 'c'
-- False

 
-- 2) Programa las siguientes funciones usando recursion o composicion:
--A continuacion mostramos algunos ejemplos del uso de las funciones en ghci:
-- $> paratodo [True, False, True]
--False
-- $> paratodo [True, True]
--True
-- $> sumatoria [1, 5, -4]
--2
-- $> productoria [2, 4, 1]
--8

-- a) paratodo :: [Bool] -> Bool, que verifica que todos los elementos de una lista
--sean True.

--	CON COMPOSICION
-- paratodo :: [Bool] -> Bool 
-- paratodo ??????????????????????????????

-- 	CON RECURSION
--LIZ
paraTodo :: [Bool] -> Bool 
paraTodo [] = True
paraTodo (x:xs) = x && paraTodo xs
--GABY
paraTodo2 :: [Bool]-> Bool 
paraTodo2 [] = True
paraTodo2 (x:xs) = (x== True) && paraTodo2 xs
-- EN GHCI
-- >paratodo []
-- True
-- >paratodo [True]
-- True
-- >paratodo [False]
-- False
-- >paratodo [True,False, True]
-- False

-- b) sumatoria :: [Int] -> Int, que calcula la suma de todos los elementos de una
--lista de enteros.

--	CON COMPOSICION
-- sumatoria :: [Int] -> Int
-- sumatoria ??????

-- 	CON RECURSION
--LIZ y GABY
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

-- c) productoria :: [Int] -> Int, que calcula el producto de todos los elementos una
--la lista de enteros.
--
--	CON COMPOSICION
-- productoria :: [Int] -> Int
-- productoria  ??????
--
-- 	CON RECURSION
-- LIZ Y GABY
productoria :: [Int] -> Int
productoria [] = 1
productoria (x:xs) = x * productoria xs

-- d)  factorial :: Int -> Int, que toma un numero n y calcula n!.
--LIZ
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)
--GABY
factorial2:: Int-> Int
factorial2 1=1
factorial2 x= x* factorial (x-1)

-- e) promedio :: [Int] -> Int, que toma una lista de numeros y calcula el valor pro-
--medio (truncado, usando divisicion entera). 
--LIZ y GABY
promedio :: [Int] -> Int
promedio xs = div (sumatoria xs) (length xs)


-- 3) Verifica si un numero pertenece pertenece :: Int -> [Int] -> Bool, que verifica si un numero se
--encuentra en una lista.
--Ejemplos de uso en ghci:
-- $> pertenece 4 [2,4,6]
--True
-- $> pertenece 6 [2,4,6]
--True
-- $> pertenece 7 [2,4,6]
--False
--LIZ
pertenece ::Int -> [Int] -> Bool
pertenece n [] = False
pertenece n (x:xs) | n == x = True
                   | n /= x  = pertenece n xs
--GABY   
pertenece2 :: Int -> [Int] -> Bool
pertenece2 n [] = False
pertenece2 n (x:xs)= (n==x)|| pertenece n xs    


-- 4) Programa la funcion encuentra que dado un valor n de tipo Int y una lista de pares de
--tipo [(Int, String)] que asocia numeros a palabras, devuelve la palabra correspondiente
--a n, es decir, el segundo componente del par cuyo primer componente es igual a n. En caso
--que exista mas de una palabra asociada al mismo numero, devuelve la primera de ellas. En
--caso que no exista palabra asociada al numero, devuelve la palabra vacia (""). Por ejemplo:
--encuentra 10 [(40, "tos"), (10, "uno"), (16, "taza"), (10, "dos")] = "uno"
--encuentra 102 [(40, "tos"), (103, "vela"), (16, "taza")] = ""
--encuentra 102 [] = ""
--LIZ Y GABY
encuentra :: Int -> [(Int,String)] -> String
encuentra n [] = ""
encuentra n ((x,y):xs) | n == x  = y 
                       | n /= x  = encuentra n  xs


-- 5. Programa las siguientes funciones que implementan los cuanticadores generales. Nota que
--el segundo parametro de cada funcion, es otra funcion!                      
--Ejemplos en ghci:
-- $> paratodo' [0,0,0,0] esCero
--True
-- $> paratodo' [0,0,1,0] esCero
--False
-- $> paratodo' "hola" esVocal
--False
-- $> existe' [0,0,1,0] esCero
--True
-- $> existe' "hola" esVocal
--True
-- $> existe' "tnt" esVocal
--False

--a) paratodo' :: [a] -> (a -> Bool) -> Bool, dada una lista xs de tipo [a] y un
--predicado t :: a -> Bool, determina si todos los elementos de xs satisfacen el
--predicado t.
--LIZ y GABY
paratodo' :: [a] -> ( a -> Bool) -> Bool
paratodo' [] t = True
paratodo' (x:xs) t = t x && paratodo' xs t
--Ejemplos en ghci:
-- $> paratodo' [0,0,0,0] esCero
-- True
-- $> paratodo' [0,0,1,0] esCero
-- False
-- $> paratodo' "hola" esVocal

--b) existe' :: [a] -> (a -> Bool) -> Bool, dada una lista xs de tipo [a] y un
--predicado t :: a -> Bool, determina si algun elemento de xs satisface el predicado
--t.  
--LIZ T GABY
existe' :: [a] -> (a -> Bool) -> Bool
existe' [] t = False
existe' (x:xs) t = t x || existe' xs t
--Ejemplos en ghci:
-- $> existe' [0,0,1,0] esCero
--True
-- $> existe' "hola" esVocal
--True
-- $> existe' "tnt" esVocal
--False

--c) sumatoria' :: [a] -> (a -> Int) -> Int, dada una lista xs de tipo [a] y una
--funcion t :: a -> Int (toma elementos de tipo a y devuelve enteros), calcula la
--suma de los valores que resultan de la aplicacion de t a los elementos de xs.  
sumatoria' :: [a] -> (a -> Int) -> Int
sumatoria' [] t = 0
sumatoria' (x:xs) t = t x + sumatoria' xs t
--Ejemplos en ghci:
-- $> sumatoria' [5, 10] factorial
-- 3628920

--d) productoria' :: [a] -> (a -> Int) -> Int, dada una lista de xs de tipo [a]
--y una funcion t :: a -> Int, calcula el producto de los valores que resultan de la
--aplicacion de ta los elementos de xs.
productoria' :: [a] -> (a-> Int ) -> Int
productoria' [] t = 1
productoria' (x:xs) t = t x * productoria' xs t
--Ejemplos en ghci:
-- $> productoria' [5, 10] factorial
-- 435456000


-- 6. Defini nuevamente la funcion paratodo, pero esta vez usando la funcion paratodo' (sin
--recursion ni analisis por casos!).
paratodo'' :: [Bool] -> Bool
paratodo'' xs = paratodo' xs (==True)
--Ejemplos en ghci:
-- $> paratodo'' [True, False]
-- False


--7. Utilizando las funciones del ejercicio 5, programa las siguientes funciones por composicion,
--sin usar recursion ni analisis por casos.

-- a) todosPares :: [Int] -> Bool verifica que todos los numeros de una lista sean
--pares.
--funcion auxiliar
esPar :: Int -> Bool
esPar x = mod x 2 == 0

todosPares :: [Int]-> Bool
todosPares xs= (paratodo' xs esPar)
--En ghci
-- $> esPar 0
-- True
-- $> esPar 5
-- False

--En ghci
-- $> todosPares [2, 6, 8]
-- True
-- $> todosPares [2..10]
-- False
-- $> todosPares [2, 5, 8]
-- False

--b) hayMultiplo :: Int -> [Int] -> Bool verifica si existe algun numero dentro del
--segundo parametro que sea multiplo del primer parametro.

--funcion auxiliar
multiplo :: Int -> Int -> Bool
multiplo n x = x `mod` n == 0

hayMultiplo :: Int-> [Int]-> Bool
hayMultiplo n xs= (existe' xs (multiplo n))
--En ghci
-- $> multiplo 2 6
-- True
-- $> multiplo 6 2
-- False

--En ghci
-- $> hayMultiplo 2 [2, 4, 6]
-- True
-- $> hayMultiplo 3 [2, 4, 8]
-- False

--c) sumaCuadrados :: Int -> Int, dado un numero no negativo n, calcula la suma de
--los primeros n cuadrados, es decir (sumatoria i : 0 <= i < n : i^2).
-- Ayuda: En Haskell se puede escribir la lista que contiene el rango de numeros entre n
--y m como [n..m].

--funcion auxiliar
cuadrado ::Int-> Int
cuadrado x= x*x 

sumaCuadrados :: Int -> Int
sumaCuadrados n = sumatoria' [0..n] cuadrado
--En ghci
-- $> cuadrado 2
-- 4
-- $> cuadrado 3
-- 9
--
-- $> sumaCuadrados 2
-- 5
-- $> sumaCuadrados 3
-- 14

--d) >Se te ocurre como redefinir factorial (ej. 2d) para evitar usar recursion?
factorial':: Int-> Int
factorial' n= productoria [1..n] 
--En ghci
-- $>factorial' 1
-- 1
-- $>factorial' 5
-- 120

--e) multiplicaPares :: [Int] -> Int que calcula el producto de todos los numeros
--pares de una lista. 

--Funcion auxiliar
pares:: [Int]-> [Int]
pares []=[]
pares (x:xs)| (mod x 2 ==0) = x: pares xs
            | otherwise = pares xs
 
multiplicaPares:: [Int]-> Int
multiplicaPares xs= (productoria' (pares xs)) (id)
--En ghci
-- $>pares [0, 3, 2]
-- [0, 2]
-- $>pares [0, 2]
-- [0, 2] 
-- $>multiplicaPares [2, 3]
-- 2
-- $>multiplicaPares [2, 3, 6]
-- 12


--8. Indaga en Hoogle (no es un typo!) sobre las funciones map y filter. 
--Tambien podes consultar su tipo en ghci con el comando :t.


-- * Que hacen estas funciones?

-- //////////////////////////////////////////////////////////////////////////////////////
-- //// ------------------------------MAP------------------------------------------------------
-- //// En ghci :t map
-- //// Tipo
-- //// map :: (a -> b) -> [a] -> [b].
-- //// -- En hoogle
-- //// --map  f xs es la lista obtenida aplicando f a cada elemento de xs , es decir,
-- //// --map f [x1, x2, ..., xn] == [f x1, f x2, ..., f xn]
-- //// --map f [x1, x2, ...] == [f x1, f x2, ...]
-- //// -------------------INTRO ALG PRACTICO 2 Ejercicio 4.-------------------------
-- //// --4. Una funcion de map es aquella que dada una lista devuelve otra lista cuyos elementos son los que se
-- //// --obtienen de aplicar una funcion a cada elemento de la primera en el mismo orden y con las mismas
-- //// --repeticiones (si las hubiere). Por ejemplo: duplica : [Int] ! [Int] devuelve cada elemento de la lista
-- //// --multiplicado por 2.
-- ///////////////////////////////////////////////////////////////////////////////////////
--Define recursivamente las siguientes funciones de map.
--a) sumar1 : [Int] -> [Int], que dada una lista de enteros le suma uno a cada uno de sus elementos.
--Por ejemplo: sumar1:[3; 0;-2] = [4; 1;-1]
--sumar1 :: [Int]-> [Int]
--sumar1 [] = []
--sumar1 (x:xs) = (x+1) : sumar1 xs
--b) duplica : [Int] ! [Int], que dada una lista de enteros duplica cada uno de sus elementos.
--Por ejemplo: duplica:[3; 0;-2] = [6; 0;-4]
--duplica :: [Int] -> [Int] 
--duplica [] = []
--diplica (x:xs) = 2 * x :duplica xs
--c) multiplica : Int -> [Int] -> [Int], que dado un numero n y una lista, multiplica cada uno de los
--elementos por n.
--Por ejemplo: multiplica:3:[3; 0;-2] = [9; 0;-6]
--multiplica :: Int -> [Int] -> [Int]
--multiplica n [] = []
--multiplica n (x:xs) = (n * x) : multiplica n xs
--Preguntas:
--a) >Se te ocurre algun otro ejemplo de una funcion de este tipo?
-- ///////////////////////////////////////////////////////////////////////////////
-- //// --FILTER---------------------------------------------------------------------
-- //// -- En ghci :t filter
-- //// -- Tipo
-- //// --filter :: (a -> Bool) -> [a] -> [a]. 
-- //// -- En hoogle
-- //// --filter , aplicado a un predicado y una lista, devuelve la lista de aquellos elementos 
-- //// --que satisfacen el predicado; es decir,
-- //// --filter p xs = [x | x <- xs, px]
-- //// -------------------INTRO ALG PRACTICO 2 Ejercicio 3.-------------------------
-- //// --3. Una funcion de filter es aquella que dada una lista devuelve otra lista cuyos elementos son los elementos
-- //// --de la primera que cumplan una determinada condicion, en el mismo orden y con las mismas repeticiones
-- //// --(si las hubiere). Por ejemplo: soloPares : [Int] -> [Int] devuelve aquellos elementos de la lista que son
-- //// --pares.
-- ///////////////////////////////////////////////////////////////////////////////
--Define recursivamente las siguientes funciones filter.
--a) soloPares : [Int] -> [Int], que dada una lista de enteros xs devuelve una lista solo con los numeros
--pares contenidos en xs, en el mismo orden y con las mismas repeticiones (si las hubiera).
--Por ejemplo: soloPares:[3; 0;-2; 12] = [0;-2]
--pares:: [Int]-> [Int]
--pares []=[]
--pares (x:xs)| (mod x 2 ==0) = x: pares xs
--            | otherwise = pares xs
--b) mayoresQue10 : [Int] -> [Int], que dada una lista de enteros xs devuelve una lista solo con los numeros
--mayores que 10 contenidos en xs,
--Por ejemplo: mayoresQue10:[3; 0;-2; 12] = [12]
--mayoresQue10 : [Int] -> [Int]
--mayoresQue10 [] = []
--mayoresQue10 (x:xs) = (x > 10) = x : mayoresQue10 xs
--c) mayoresQue : Int -> [Int] -> [Int], que dado un entero n y una lista de enteros xs devuelve una lista
--solo con los numeros mayores que n contenidos en xs,
--Por ejemplo: mayoresQue:2:[3; 0;-2; 12] = [3; 12]
--mayoresQue : Int -> [Int] -> [Int]
--mayoresQue n [] = []
--mayoresQue n (x:xs) = (x > n) = x : mayoresQue xs
--Preguntas:
--a) >Se te ocurre algun otro ejemplo de una funcion de este tipo?

-- * A que equivale la expresion map succ [1, -4, 6, 2, -8], donde succ n = n+1?
-- /////////////////////////////////////////////////////////////////////////////
-- //// -- En ghci
-- //// -- $> map succ [1, -4, 6, 2, -8]
-- //// -- [2, -3, 7, 3, -7]
-- //// --La expresión map succ (sucesion de numeros domde succ n = n+1)equivale 
-- //// --a aplicar succ a cada elemento de forma independiente. 
-- //////////////////////////////////////////////////////////////////////////////

-- * Y la expresion filter esPositivo [1, -4, 6, 2, -8]?
-- //////////////////////////////////////////////////////////////////////////////
-- //// -- En ghci
-- //// -- $> filter esPositivo [1, -4, 6, 2, -8]
-- //// -- [1, 6, 2]
-- //// -- La expresión filter esPositivo filtra todos los elementos positivos de esa lista. 
-- /////////////////////////////////////////////////////////////////////////////

--DUPLICA CON MAP
--
--duplica : [Int] -> [Int] 
--duplica xs = map t xs where (t == 2*x)
--



--9. Programa una funcion que dada una lista de numeros xs, devuelve la lista que resulta de
--duplicar cada valor de xs.

--a) Definila usando recursion.
duplica :: [Int] -> [Int] 
duplica [] = []
duplica (x:xs) = 2 * x : duplica xs
--b) Definila utilizando la funcion map.
--LIZ Y GABY
duplica' :: [Int]->[Int]
duplica' xs = map(*2) xs

--duplicamap : [Int] -> [Int] 
--duplicamap xs = map t xs Where (t == *2)


--10. Programa una funcion que dada una lista de numeros xs, calcula una lista que tiene como
--elementos aquellos numeros de xs que son pares.

--a) Definila usando recursion.
soloPares:: [Int]->[Int]
soloPares [] =[]
soloPares (x:xs)|(mod x 2 ==0) = x: soloPares xs 
                |(mod x 2 /=0) = soloPares xs

--b) Definila utilizando la funcion filter.
soloPares' :: [Int]->[Int]
soloPares' []=[]
soloPares' xs= filter(even) xs

--c) Revisa tu definicion del ejercicio 7e. Como podes mejorarla?
multiplicaPares' :: [Int]-> Int
multiplicaPares' xs= productoria' (soloPares' xs) (id)


--11. Considera las siguientes funciones:

--a) Programa las funciones usando recursion.

-- * sumarALista :: Num a => a -> [a] -> [a] que toma un numero y una lista de
--numeros y le suma a cada elemento de la lista el primer parametro. Por ejemplo:
-- sumarALista 3 [4,6,7] = [7,9,10]
sumarAlistas :: Num a => a -> [a]-> [a]
sumarAlistas n []= []
sumarAlistas n (x:xs)= (n+x): sumarAlistas n xs
-- * encabezar :: a -> [[a]] -> [[a]] que toma un valor de tipo a y lo introduce en
--la cabeza de cada lista del segundo parametro. Por ejemplo:
-- encabezar 3 [[2,1],[],[4,7]] = [[3,2,1],[3],[3,4,7]]
encabezar :: a -> [[a]] -> [[a]]
encabezar n [] = []
encabezar n (xs:xss)= ((n:xs):encabezar n xss) 
-- * mayoresA :: Ord a -> a -> [a] -> [a] que toma un valor ordenable n y una
--lista de valores ordenables xs, y calcula una lista que contiene los elementos de xs que
--son mayores que n
-- mayoresA 4 [1,2,3,4,5,6,7,8,9] = [5,6,7,8,9]
mayoresA :: Ord a => a -> [a] -> [a]
mayoresA n [] = []
mayoresA n (x:xs) |(x>n) = x:mayoresA n xs
                  |(x<=n) = mayoresA n xs 

--b) Programa las funciones utilizando map y filter segun corresponda.

-- * sumarALista :: Num a => a -> [a] -> [a] que toma un numero y una lista de
--numeros y le suma a cada elemento de la lista el primer parametro. Por ejemplo:
-- sumarALista 3 [4,6,7] = [7,9,10]
sumarAlistas' :: Num a => a -> [a]-> [a]
sumarAlistas' n xs= map (+n) xs
-- * encabezar :: a -> [[a]] -> [[a]] que toma un valor de tipo a y lo introduce en
--la cabeza de cada lista del segundo parametro. Por ejemplo:
-- encabezar 3 [[2,1],[],[4,7]] = [[3,2,1],[3],[3,4,7]]
encabezar' :: a -> [[a]] -> [[a]]
encabezar' n xss = map(n:) xss
-- * mayoresA :: Ord a -> a -> [a] -> [a] que toma un valor ordenable n y una
--lista de valores ordenables xs, y calcula una lista que contiene los elementos de xs que
--son mayores que n
-- mayoresA 4 [1,2,3,4,5,6,7,8,9] = [5,6,7,8,9]
mayoresA':: Ord a => a -> [a] -> [a]
mayoresA' n xs = filter(n<) xs 


--12. >Se te ocurre como programar la funcion del ejercicio 4 utilizando composicion y la funcion
--filter?
condicion :: Int -> (Int, String)-> Bool
condicion n (a,b)= (n==a)

separa :: [(Int, String)]->String
separa ((a,b):xs)=b

encuentra' :: Int -> [(Int,String)]->String
encuentra' n xs = separa (filter (condicion n) xs)

--13. La funcion primIgualesA toma un valor y una lista, y calcula el tramo inicial mas largo de
--la lista cuyos elementos son iguales a ese valor. Por ejemplo:

--primIgualesA 3 [3,3,4,1] = [3,3]
--primIgualesA 3 [4,3,3,4,1] = []
--primIgualesA 3 [] = []
--primIgualesA 'a' "aaadaa" = "aaa"

--a) Programa primIgualesA por recursion.
primIgualesA :: (Eq a)=> a-> [a]-> [a]
primIgualesA n []= []
primIgualesA n (x:xs)|(n==x)= x: primIgualesA n xs
                     |(n/=x)= []

--b) Programa nuevamente la funcion utilizando takeWhile.
primIgualesA':: (Eq a)=> a-> [a]-> [a]
primIgualesA' n xs= takeWhile (n==) xs

--14. La funcion primIguales toma una lista y devuelve el mayor tramo inicial de la lista cuyos
--elementos son todos iguales entre si. Por ejemplo:

--primIguales [3,3,4,1] = [3,3]
--primIguales [4,3,3,4,1] = [4]
--primIguales [] = []
--primIguales "aaadaa" = "aaa"

--a) Programa primIguales por recursion.
primIguales :: (Eq a)=>[a]-> [a]
primIguales []=[]
primIguales [x]=[x]
primIguales (x:y:xs) |(x/=y)=[x]
                     |(x==y)= [x,y]

--b) Usa cualquier version de primIgualesA para programar primIguales sin recursion
primIguales' :: (Eq a)=>[a]-> [a]
primIguales' []=[]
primIguales' (x:xs)= primIgualesA x (x:xs)

--15. Considera la funcion minimo que calcula cual es el menor valor de una lista de tipo [a].

--a) Definila solo para listas no vacias.
obtMin:: (Ord a)=> a->[a]-> a
obtMin a []= a
obtMin a (x:xs) = obtMin (min a x) xs

minimo :: (Ord a)=> [a]-> a
minimo (x:xs) = obtMin x xs

--b) Definila para todos los casos, limitando el tipo a a la clase Bounded para poder definir
--el caso base.
--Ayuda: Para probar esa funcion dentro de ghci con listas vacias, indicar el tipo concreto
--con tipos de la clase Bounded, por ejemplo: ([1,5,10]::[Int]), ([]::[Bool]), etc.
minimo' :: (Bounded a, Ord a) => [a] -> a
minimo' [] = maxBound
minimo' (x:xs)= min x (minimo' xs)


--16. (*) Para cada uno de los siguientes patrones, decidi si estan bien tipados, y en tal caso da los
--tipos de cada subexpresion. En caso de estar bien tipado, >el patron cubre todos los casos
--de definicion?

--a) f :: (a, b) -> ...
--f x = ...
--a)-esta mal  tipado debería ser: f::(a,b)->...
--                                 f (x,y)=...

--b) f :: (a, b) -> ...
--f (x , y) = ...
--b)- esta bien tipado y cubre todo los casos. a y b son  polimorfismos paramétricos, es decir son  variables de tipo.

--c) f :: [(a, b)] -> ...
--f (a , b) = ...
--c)- esta mal tipado y no cubre todos los casos. f::(a,b)->  tipo:a y b son  polimorfismos paramétricos, es decir son  variables de tipo.
--                                                f (a,b):xs= 

--d) f :: [(a, b)] -> ...
--f (x:xs) = ...
--d)-esta bien tipado. tipo:a y b son  polimorfismos paramétricos, es decir son  variables de tipo


--e) f :: [(a, b)] -> ...
--f ((x, y) : ((a, b) : xs)) = ...
--e)-esta mal tipado y no cubre todos los casos. f:: [(a,b)]-> tipo:a y b son  polimorfismos paramétricos, es decir son  variables de tipo
--                                               f ((x,y):(a,b):xs)=

--f) f :: [(Int, a)] -> ...
--f [(0, a)] = ...
--f)-esta mal tipada y no cubre todos los casos     f::[(Int,a)]-> tipo:Int y a variable de tipo . 
--                                                  f ((x,y):xs)=

--g) f :: [(Int, a)] -> ...
--f ((x, 1) : xs) = ...
--g)-esta bien tipada pero no cubre todos los casos f::[(Int,a)]->tipo:Int y a variable de tipo .
--                                                  f::((x,y):xs)

--h) f :: [(Int, a)] -> ...
--f ((1, x) : xs) = ...
--h)si esta bien tipado pero no cubre todos los casos f [(Int, a)] ->tipo:Int y a variable de tipo 
--						      f ((x,y), xs)) =

--i) f :: (Int -> Int) -> Int -> ...
--f a b = ...
--i) Esta bien tipada y cubre todos los casos f :: (Int -> Int) -> Int -> ...Todos de tipo Int
--					      f  a b = ...

--j) f :: (Int -> Int) -> Int -> ...
--f a 3 = ...
-- j) Esta bien tipado pero no cubre todos los casos f :: (Int -> Int) -> Int -> ....Todos de tipo Int
--					      f  a b = ...

--k) f :: (Int -> Int) -> Int -> ...
--f 0 1 2 = ...
-- k)Esta mal tipada y no cubre todos los casos f :: (Int -> Int) -> Int -> ...Todos de tipo Int
--					      f  x y = ...

--l) f :: a -> (a -> a) -> ...
--f a g = ...
--l) Esta bien tipada y cubre todos los casos f :: Int ->(Int -> Int) ->  ...Todos de tipo Int
--					      f a g =


--17. (*) Para las siguientes declaraciones de funciones, da al menos una defincion cuando sea
--posible (que no sea la expresion undefined). >Podes dar alguna otra definicion alternativa
--a la que diste en cada caso?

--a) f :: (a, b) -> a
--fst:: (a,b)->a. Dada una tupla, devuelve el primer elemento de la misma.

--b) f :: (a, b) -> b
--snd::(a,b)-> a. Dada una tupla, duvuelve el segundo elemento de la misma.

--c) f :: (a, b) -> c
--sumatupla::(a,b)->c. Dada una tupla, duvuelve la suma de los elementos de la misma.

--d) f :: a -> b
--por2 ::a->b. Dado un elemento a, lo duplica

--e) f :: (a -> b) -> a -> b
--invPor2 ::(a->b)->a->b.Toma la función por2 y la aplica al inverso de a.

--f) f :: (a -> b) -> [a] -> [b]
--todos0::(a->b)->[a]->[b]. Dada una lista devuelve True si todos sus elemento son 0.

--g) f :: (a -> b) -> a -> c

--h) f :: (a -> b) -> (b -> c) -> a -> c




-- /////////////////////////////////////////////////////////////////////////////
-- Stevent Holkins realizo una demostracion de que 1+1 = 3 o era que 2+2 = 5
-- En el 2004 en FAMAF yo realice una demostracion sintetizada de esa demostracion
-- Hoy en el 2019 frenmte a los debates a principio del 2019 que eso no es programable
-- y luego de promocionar intro con 8 
-- El 02 de septiembre a las 3:00 am decidi hacerla en un programna 
-- En dos versiones
-- VERSION 1
-- toma dos numeros y devuelve el siguiente de su resultado
steventHolkins :: Int -> Int -> Int
steventHolkins a b = succ (a + b) 
-- En ghci
-- $>steventHolkins 1 1
-- 3
-- $>steventHolkins 2 2
-- 5
-- VERSION 2
steventHolkins2 :: Int -> Int
steventHolkins2 a = succ (a + a)
-- En ghci
-- $> steventHolkins 1
-- 3
-- $> steventHolkins 2
-- 5
-- /////////////////////////////////////////////////////////////////////////////











 


