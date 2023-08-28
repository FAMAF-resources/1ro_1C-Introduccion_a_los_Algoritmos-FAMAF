{- ////////////////////////////////////////////// -}
                {- GUIA 2 -}
    {- Autor: Valentina S. Vispo - 2019 -}
{- ////////////////////////////////////////////// -}

{- Guia 2 Ejercicio 3: Una funcion de filter es 
aquella que dada una lista devuelve otra lista cuyos 
elementos son los elementos de la primera que 
cumplan una determinada condicion, en el mismo 
orden y con las mismas repeticiones (si las hubiere). 
Por ejemplo:soloPares: [Int]→[Int] devuelve 
aquellos elementos de la lista que son pares. 
Definı recursivamente las siguientes funciones filter -}

{- a)soloPares: [Int]→[Int], que dada una lista de 
enteros xs devuelve una lista solo con los numeros 
pares contenidos en xs, en el mismo orden y con las
mismas repeticiones (si las hubiera).
Por ejemplo:soloPares.[3,0,−2,12] = [0,−2] -}

{- soloPares :: [Int] -> [Int]
soloPares [] = []
soloPares (x:xs) | x mod 2 == 0 = x : (soloPares xs)
                 | otherwise = soloPares xs -}

{- soloPares 1 : [2,3]
= {def de soloPares}
soloPares 2 : [3]
= {def de soloPares}
2 : (soloPares [3])
= {def de soloPares}
2 : (soloPares [])
= {def de soloPares}
2 : ([])
=
[2] -}

{- ////////////////////////////////////////////// -}

{- Guia 2 Ejercicio 4: Una funcion de map es aquella 
que dada una lista devuelve otra lista cuyos elementos 
son los que seobtienen de aplicar una funcion a cada 
elemento de la primera en el mismo orden y con las mismas 
repeticiones (si las hubiere). 
Por ejemplo: duplica : [Int] ! [Int] devuelve cada elemento 
de la lista multiplicado por 2. Definı recursivamente las 
siguientes funciones de map. -}

{- a)sumar1: [Int]→[Int], que dada una lista de 
enteros le suma uno a cada uno de sus elementos.
Por ejemplo:sumar1.[3,0,−2] = [4,1,−1] -}

sumar1' :: [Int] -> [Int] 
sumar1' [] =[] 
sumar1' (x:xs) = (x+1) : (sumar1' xs)

{- [1,2,3] -> [2,3,4]
2> 1:[2,3] = 1+1 : (sumar [2,3] -}

{- b)duplica: [Int]→[Int], que dada una lista de 
enteros duplica cada uno de sus elementos.
Por ejemplo:duplica.[3,0,−2] = [6,0,−4] -}

duplica:: [Int] -> [Int] 
duplica [] = [] 
duplica (x:xs) = (x*2):(duplica xs)

{- COMO HACER DUPLICA

Si la lista esta vacia: dar una lista vacia
Si no, hay al menos un primero. Multiplico por dos 
el primero y lo agrego a la lista que resulta de 
duplicar los que quedan

duplica [1,2,3]
-}

{- c)multiplica:Int→[Int]→[Int], que dado un numero
n y una lista,  multiplica cada uno de los elementos
por n.Por ejemplo:multiplica.3.[3,0,−2] = [9,0,−6] -}

multiplica :: Int -> [Int] ->[Int] 
multiplica n [] =[] 
multiplica n (x:xs) = (x*n):(multiplica n xs)

{- ////////////////////////////////////////////// -}

{- Guia 2 Ejercicio 5: Una funcion de foldes aquella
que dada una lista devuelve un valor resultante de 
combinar los elementos de la lista. 
Por ejemplo:sum: [Int] → Int devuelve la sumatoria 
de los elementos de la lista. Definı recursivamente 
las siguientes funciones fold -} 

{- a)todosMenores10: [Int]→Bool, que dada una lista 
devuelve True si esta consiste solo de numeros 
menores que 10 -}

todosmenores10 :: [Int] -> Bool
todosmenores10 []= True
todosmenores10 (x:xs) | (x<10) == (todosmenores10 xs) = True 
              | (x>=10) == (todosmenores10 xs) = False

{- [1,2,3] -> True
[10, 8, 1] -> False -}

{- b)hay0: [Int]→Bool, que dada una lista decide si
existe algun 0 en ella -}

hay0 :: [Int] -> Bool
hay0 [] = False 
hay0 (x:xs) | (x==0)==(hay0 xs) = True
        | (x/=0)==(hay0 xs) = False

{- [1,0,1]-> True
[1,1,1]-> False -}
 
{- c)sum2: [Int]→Int, que dada una lista devuelve 
la suma de todos sus elementos -}
sum2 ::[Int] -> Int
sum2 [] = 0
sum2 (x:xs)= (x+ (sum2 xs))

{- ////////////////////////////////////////////// -}

{- Guia 2 Ejercicio 6) Funcion tipo ZIP -}

{- Una funcion de tipo zip es aquella que dadas dos 
listas devuelve una lista de pares donde el primer 
elemento de cada par se corresponde con la primera 
lista, y el segundo elemento de cada par se corresponde 
con la primera lista. Por ejemplo: 
repartir : [String] ! [String] ! [(String; String)] 
donde los elementos de la primera lista son nombres
de personas y los de la segunda lista son cartas 
espa~nolas es una funcion que devuelve una lista 
de pares que le asigna a cada persona una carta.
Ej: repartir:["Juan"; "Maria"]:["1deCopa"; "3deOro"; 
"7deEspada"; "2deBasto"] = [("Juan"; "1deCopa"); 
("Maria"; "3deOro")]
Defina la funcion recursivamente. -}

repartir :: [String] -> [String] -> [(String,String)]
repartir [] ys = []
repartir xs [] = []
repartir (x:xs) (y:ys) = (x,y):(repartir xs ys)  

{- dadas dos listas devuelve una lista de pares -}
{- [(*String*,String)] donde el primer elemento
de cada par se corresponde con la primera lista -}
{- [(String,*String*)] y el segundo elemento de 
cada par se corresponde con la primera lista -}
{- ["Pedro", "Rober"] -> ["Pato", "Vaca"] -> 
[("Pedro","Pato"),("Rober", "Vaca")] -}

{- ////////////////////////////////////////////// -}

{- Guia 2 Ejercicio 7) Funcion tipo UNZIP -}

{- Una funcion de tipo unzip es aquella que dada 
una lista de tuplas devuelve una lista de alguna 
de las proyecciones de la tupla. Por ejemplo, 
si tenemos una lista de ternas donde el primer 
elemento representa el nombre de un alumno, 
el segundo el apellido, y el tercero la edad, 
la funcion que devuelve la lista de todos los
apellidos de los alumnos en una de tipo unzip.
Definir la funcion 
apellidos : [(String; String; Int)] ! [String]
Ej: apellidos:[("Juan"; "Dominguez"; 22); 
("Maria"; "Gutierrez"; 19); ("Damian"; "Rojas"; 18)] =
["Dominguez""Gutierrez"; "Rojas"]
Defina la funcion recursivamente. -}

apellidos :: [(String, String, Int)] -> [String]
apellidos [] = []
apellidos ((n, a, e):xs) = a : (apellidos xs)

{- COMO TOMAR LOS APELLIDOS

Si esta vacia, devolve la lista vacia
Si no, agarra la primera terna, que es de la forma 
(n, a, e). Meto el a en la lista de apellidos que 
quedan.

[("Fran", "Trucco", 24), ("Dani", "Bravo", 20), 
("Valen", "Vispo", 18)] -}

{- ("Daniel", "Bravo", 20),("Valentina","Vispo",18),
("Rober", "Vaca", 100) -> ["Bravo","Vispo",Vaca"] -}

{- ////////////////////////////////////////////// -}

{- Guia 2 Ejercicio 8) -}

{- ////////////////////////////////////////////// -}

{- Guia 2 Ejercicio 9) -}

{- a) maximo : [Int] → Int, que calcula el maximo 
elemento de una lista de enteros.
Por ejemplo: maximo.[2, 5, 1, 7, 3] = 7
Ayuda: Ir tomando de a dos elementos de la lista y 
‘quedarse’ con el mayor. -}

maximo1 :: [Int] -> Int
maximo1 [] = minBound
maximo1 (x:xs) = x `max` (maximo1 xs) 

{- b) sumaPares : [(Num, Num)] → Num, que dada una 
lista de pares de n´umeros, devuelve la sumatoria
de todos los n´umeros de todos los pares.
Por ejemplo: sumaPares.[(1, 2)(7, 8)(11, 0)] = 29 -}

sumaPares2 :: [(Int, Int)] -> Int
sumaPares2 [] = 0
sumaPares2 ((x,y):xs) = (x+y) + (sumaPares2 xs) 

{- c) todos0y1 : [Int] → Bool, que dada una lista 
devuelve True si ´esta consiste s´olo de 0s y 1s.
Por ejemplo: todos0y1.[1, 0, 1, 2, 0, 1] = False, 
todos0y1.[1, 0, 1, 0, 0, 1] = True -}

todos0y1 :: [Int] -> Bool
todos0y1 [] = True
todos0y1 (x:xs) | (x==0 || x==1) = todos0y1 xs  
        | (x/=1 && x/=0) = False
        
{- d) quitar0s : [Int] → [Int] que dada una lista de enteros 
devuelve la lista pero quitando todos los ceros.
Por ejemplo quitar0s.[2, 0, 3, 4] = [2, 3, 4] -}        
        
quitar0s :: [Int] -> [Int]
quitar0s [] = []
quitar0s (x:xs) | x==0 = (quitar0s xs)
        | x/=0 = x: (quitar0s xs)

{- e) ultimo : [A] → A, que devuelve el ultimo elemento 
de una lista. Por ejemplo: ultimo.[10, 5, 3, 1] = 1 -}

ultimo :: [a] -> a
ultimo [x] = x
ultimo (x:xs) = ultimo xs

{- f ) repetir : Num → Num → [Num], que dado un numero n 
mayor o igual a 0 y un n´umero k arbitrario
construye una lista donde k aparece repetido n veces.
Por ejemplo: repetir.3.6 = [6, 6, 6] -}

repetir :: Int -> Int -> [Int]
repetir 0 k = []  
repetir n k = k:(repetir (n-1) k) 

{- n>=0 -> k "arbitrario" -> [donde k aparece 
repetido n veces] -}

{- g) concat : [[A]] → [A] que toma una lista de 
listas y devuelve la concatenaci´on de todas ellas.
Por ejemplo: concat.[[1, 4], [], [2]] = [1, 4, 2] -}

concate :: [[a]] -> [a]
concate [] = []
concate (x:xs) = x ++ concate xs

{- h) rev : [A] → [A] que toma una lista y devuelve 
una lista con los mismos elementos pero en orden
inverso.
Por ejemplo: rev.[1, 2, 3] = [3, 2, 1] -}

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]

{- ////////////////////////////////////////////// -}

{- Guia 2 Ejercicio 21: (i) Definı funciones por 
recursion para cada una de las siguientes 
descripciones. 
(ii) Evalua los ejemplos manualmente 
(iii) Identifica si las funciones son de algun tipo 
ya conocido (filter, map, fold). 
(iv)
Programalas en haskell y verifica los resultados
obtenidos. -}
 
{- a) listasIguales : [A] → [A] → Bool, que determina 
si dos listas son iguales, es decir, contienen los
mismos elementos en las mismas posiciones 
respectivamente.
Por ejemplo: listasIguales.[1, 2, 3].[1, 2, 3] = True, 
listasIguales.[1, 2, 3, 4].[1, 3, 2, 4] = False

listasIguales :: [a] -> [a] -}

{- b) mejorNota : [(String,Int,Int,Int)] → 
[(String,Int)], que selecciona la nota mas alta 
de cada alumno.
Por ejemplo: mejorNota.[(“Matias”,7,7,8),(“Juan”,10,6,9),
(“Lucas”,2,10,10)] = [(“Matias”,8),(“Juan”,10), (“Lucas”,10)]. -}

{- c) incPrim : [(Int,Int)] → [(Int,Int)], que dada 
una lista de pares de enteros, le suma 1 al primer
numero de cada par.
Por ejemplo: 
incPrim.[(20, 5),(50, 9)] = [(21, 5),(51, 9)],
 incPrim.[(4, 11),(3, 0)] = [(5, 11),(4, 0)]. -}
 
{- d) expandir : String → String, pone espacios 
entre cada letra de una palabra.
Por ejemplo: 
expandir."hola" = "h o l a" (¡sin espacio al final!) -}

{- ////////////////////////////////////////////// -}

{-
qsort [] = []
qsort (x:xs) = qsort small ++ mid ++ qsort large
  where
    small = [y | y<-xs, y<x]
    mid   = [y | y<-xs, y==x] ++ [x]
    large = [y | y<-xs, y>x]

fib 0 = 0
fib 1 = 1
fib n = (fib (n-2)) + (fib (n-1)) -}