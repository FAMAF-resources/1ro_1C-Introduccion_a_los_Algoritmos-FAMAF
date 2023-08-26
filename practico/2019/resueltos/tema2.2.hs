{-
Examen del proyecto 2
Tema 1

Ejercicio 1:
**Definir un tipo Mascota, que tiene como constructores Perro , Gato y Pez donde cada constructor toma un String de parámetro que se refiere a la raza/variedad del animal. 

Definir un tipo TiendaMascotas que es una lista de Mascota a la venta, con constructores TiendaVacia y AgregarMascota. **

Una vez definido el tipo, cree la tienda Puki, que tiene un pez payaso, un perro labrador y un gato de angora es. Utilice una función constante para definir la tienda tienda_Puki :: TiendaMascotas.

**Ejercicio 2:
Utilizando lo programado en el proyecto 2 implementá las siguientes funciones:
cuantos_estudiantes :: Carrera -> [Persona] -> Int
Que devuelve la cantidad de personas que son estudiantes de la carrera especificada. Prográmela utilizando pattern matching.
Reprograme la función anterior utilizando filter.
-}

                        --Ejercicio 1--

type Raza_Variedad = String

data Mascota= Perro Raza_Variedad
            | Gato Raza_Variedad
            | Pez Raza_Variedad deriving (Eq, Show)

data TiendaMascotas= TiendaVacia
                   | AgregarMascota Mascota TiendaMascotas deriving (Eq, Show)

-- TiendoVacia :: TiendaMascotas
-- AgregarMascotas :: Mascota -> TiendaMascotas -> TiendaMascotas 

--data Persona= Per String Int Int Rol deriving (Eq,Show)
data TiendaPuki = TPuki TiendaMascotas deriving (Eq, Show)

--tienda_Puki (Pez, Perro, Gato) = [(("pez" "payaso"), ("perro" "labrador"), ("gato" "angora"))]

--type Puki = TiendaMascotas --AgregarMascota Pez "payaso" Perro "labrador" Gato "angora"

tienda_Puki::TiendaMascotas
tienda_Puki = (AgregarMascota (Pez "payaso") ((AgregarMascota (Perro "labrador") TiendaVacia) ))
--(AgregarMascota (Perro "labrador")) (AgregarMascota (Gato "angora"))  

--TiendaMascotas->MascotasAlaVenta->TiendaVacia|AgregarMascotaMascota->Mascota-> Perro|Pez|Gato


                        --Ejercicio 2--
data Carrera = Matematica | Fisica | Computacion | Astronomia | Profesorado deriving (Eq,Show)

type Ingreso = Int

--tipo enumerado
data Funcion = Teorico | Practico deriving (Eq,Show)

--tipo algebraico
data Rol = Decanx                          --constructor sin argumento
          | Docente Funcion                --constructor con un argumento
          | Estudiante Carrera Ingreso     --contructor con dos argumento
          deriving (Eq,Show)

data Persona= Per String Int Int Rol deriving (Eq,Show)


cuantos_estudiantes::Carrera->[Persona]->Int
cuantos_estudiantes c [] = 0
cuantos_estudiantes c ((Per a b d (Estudiante n _)):ps)| (c== n)= 1 + cuantos_estudiantes c ps
                                                       | (c/= n)= cuantos_estudiantes c ps

{- Para probar código
cuantos_estudiantes Fisica []
= 0
cuantos_estudiantes Matematica [(Per "Vale" 4 4 (Estudiante Astronomia 2000))]
= 0
cuantos_estudiantes Matematica [(Per "Vale" 4 4 (Estudiante Astronomia 2000)), (Per "Alf" 4 4 (Estudiante Matematica 7894))]
= 1
-}

--cuantos_estudiantes'::Carrera-> [Persona]-> Int
--cuantos_estudiantes' c ((Per a b d (Estudiante n _)):ps)= length (filter (==c) ps)
--length (filter (== c) ps)

{- Ejercicio Estrella:
Usando los ejercicios resueltos en el proyecto 2, definir una función:
cuantos_estudiantes :: Carrera -> Arbol Persona -> Int
Que devuelve la cantidad de personas son estudiantes de la carrera especificada. -}

--cuantos_estA :: Carrera -> Arbol Persona -> Int
--cuantos_estA c (A _) = 0
--cuantos_estA c (Per n d f (Estudiante c _):xs) = 1 + (cuantos_estA c xs) 
--cuantos_estA c (Per n d f (Docente _):xs) = 0 + (cuantos_estA c xs)


