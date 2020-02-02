--Proyecto 2 Definir 
--Tipos enumerados. Cuando los distintos valores que debemos distinguir en un tipo son
--finitos, podemos enumerar cada uno de los valores del tipo. Por ejemplo, podr´ıamos representar
--las carreras que se dictan en nuestra facultad con el siguiente tipo:

--Cada uno de estos valores es un constructor, ya que al utilizarlo en una expresi´on, generan un valor del tipo Carrera.
--a) Implement´a el tipo Carrera como est´a definido arriba.
data Carrera = Matematica | Fisica | Computacion | Astronomia | Profesorado
--b) Defin´ı la siguiente funci´on, usando pattern matching: titulo :: Carrera -> String
--que devuelve el nombre completo de la carrera en forma de string. Por ejemplo, para el
--constructor Matematica, debe devolver ”Licenciatura en Matem´atica”.
titulo2 :: Carrera -> String
titulo2 Matematica = "Lic en Matematica"
titulo2 Fisica = "Lic en Fisica"
titulo2 Computacion = "Lic en Computacion"
titulo2 Astronomia = "Lic en Astronomia"
titulo2 Profesorado = "Profesorado"
--c) ¿Pod´es definir la funci´on anterior usando an´alisis por casos? ¿Por qu´e?

--2. Tipos enumerados; constructores con par´ametros. En este ejercicio, introducimos dos
--conceptos: los sin´onimos de tipos y tipos algebraicos cuyos constructores llevan 
--argumentos.
--Un sin´onimo de tipo nos permite definir un nuevo nombre para un tipo ya existente, como 
--el ya conocido tipo String que no es otra cosa que un sin´onimo para [Char]. Por ejemplo, -
--  si queremos modelar el a˜no de ingreso de un estudiante a una carrera, podemos definir:
-- Ingreso es un sinonimo de tipo.
--type Ingreso = Int
--Los tipos algebraicos tienen constructores que llevan par´ametros. Esos par´ametros 
--permiten agregar informaci´on, generando potencialmente infinitos valores dentro del 
--tipo. Por ejemplo, si queremos modelar los roles de las diferentes personas que son 
--miembros de la comunidad de nuestra facultad, podr´ıamos definir los siguientes tipos: 
-- Funcion es un tipo enumerado
--data Funcion = Teorico | Practico
--Rol es un tipo algebraico
--data Rol = Decanx --constructor sin argumento
--  ---| Docente Funcion --- constructor con un argumento
--- -| Estudiante Carrera Ingreso -- constructor con dos argumentos

--a) Implement´a los tipos Ingreso, Funcion y Rol como est´an definidos arriba.

type Ingreso = Int
data Funcion = Teorico | Practico deriving(Show, Eq)
--Rol es un tipo algebraico
data Rol = Decanx --constructor sin argumento
                  | Docente Funcion --- constructor con un argumento
                  | Estudiante Carrera Ingreso deriving(Show, Eq) -- constructor con dos argumentos

--b) ¿Cu´al es el tipo del constructor Docente?

--Docente :: Funcion -> Rol

--c) Programa la funcion 
cuantos_doc :: [Rol] -> Funcion -> Int 
cuantos_doc [ ] c = 0 
cuantos_doc xs c | x == Docente c = 1 + cuantos_doc xs c deriving(Show, Eq) 
                               | otherwise = 0 + cuantos_doc xs c
--que dada una lista de roles xs, y una funcion c, devuelve la cantidad de docentes 
--incluidos en xs que tienen la funcion c. Para saber si la definiste bien, prob´a llamarla de 
--la siguiente manera:
--cuantos_doc [Decanx, Docente Teorico, Docente Practico] Teorico
--El resultado debe ser 1.

--d) ¿La funci´on anterior usa filter? Si no es as´ı, reprogramala para usarla.

--e) ¿Qu´e modificaciones se deben realizar sobre el tipo Rol para poder representar el g
-- enero del decano/a, sin agregar otro constructor? . Para hacer las modificaciones, utiliza 
--un nuevo tipo Rol’ .

--f ) ¿Podemos representar un alumno que est´a inscripto en dos carreras? Si no, ¿qu´e habr
--ıa que modificar para conseguirlo?. Si vas a hacer las modificaciones, utiliza un nuevo 
--tipo Rol’’ . Luego program´a la funci´on estudia :: Rol’’ -> Carrera -> Bool que
--dado un rol y una carrera, determina si se trata de un estudiante de dicha carrera.



