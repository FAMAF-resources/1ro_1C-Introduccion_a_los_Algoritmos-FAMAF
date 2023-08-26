{- a)sumar1: [Int]→[Int], que dada una lista de enteros le suma uno a cada uno de sus elementos.Por ejemplo:sumar1.[3,0,−2] = [4,1,−1] -}

sumar1' :: [Int] -> [Int] 
sumar1' [] =[] 
sumar1' (x:xs) = (x+1) : (sumar1' xs)

{- [1,2,3] -> [2,3,4]
2> 1:[2,3] = 1+1 : (sumar [2,3] -}

{- b)duplica: [Int]→[Int], que dada una lista de enteros duplica cada uno de sus elementos.Por ejemplo:duplica.[3,0,−2] = [6,0,−4] -}

duplica:: [Int] -> [Int] 
duplica [] = [] 
duplica (x:xs) = (x*2):(duplica xs)

{- COMO HACER DUPLICA

Si la lista esta vacia: dar una lista vacia
Si no, hay al menos un primero. Multiplico por dos el primero y lo agrego a la lista que resulta de duplicar los que quedan

duplica [1,2,3]
-}

{- c)multiplica:Int→[Int]→[Int], que dado un numero n y una lista,  multiplica cada uno de los elementos por n.Por ejemplo:multiplica.3.[3,0,−2] = [9,0,−6] -}

multiplica :: Int -> [Int] ->[Int] 
multiplica n [] =[] 
multiplica n (x:xs) = (x*n):(multiplica n xs)

{- Guia 2 Ejercicio 5: Una funcion de foldes aquella que dada una lista devuelve un valor resultante de combinar los elementos de la lista. Por ejemplo:sum: [Int] → Int devuelve la sumatoria de los elementos de la lista. Definı recursivamente las siguientes funciones fold -} 

{- a)todosMenores10: [Int]→Bool, que dada una lista devuelve True si esta consiste solo de numeros menores que 10 -}

todosmenores10 :: [Int] -> Bool
todosmenores10 []= True
todosmenores10 (x:xs) | (x<10) == (todosmenores10 xs) = True 
		      | (x>=10) == (todosmenores10 xs) = False

{- [1,2,3] -> True
[10, 8, 1] -> False -}


{- b)hay0: [Int]→Bool, que dada una lista decide si existe algun 0 en ella -}

hay0 :: [Int] -> Bool
hay0 [] = False 
hay0 (x:xs) | (x==0)==(hay0 xs) = True
	    | (x/=0)==(hay0 xs) = False

{- [1,0,1]-> True
[1,1,1]-> False -}
 
{- c)sum2: [Int]→Int, que dada una lista devuelve la suma de todos sus elementos -}
sum2 ::[Int] -> Int
sum2 [] = 0
sum2 (x:xs)= (x+ (sum2 xs))

{- 6) Funcion tipo ZIP -}

repartir :: [String] -> [String] -> [(String,String)]
repartir [] ys = []
repartir xs [] = []
repartir (x:xs) (y:ys) = (x,y):(repartir xs ys)  

{- dadas dos listas devuelve una lista de pares -}
{- [(*String*,String)] donde el primer elemento
de cada par se corresponde con la primera lista -}
{- [(String,*String*)] y el segundo elemento de cada par se corresponde con la primera lista -}
{- ["Pedro", "Rober"] -> ["Pato", "Vaca"] -> [("Pedro","Pato"),("Rober", "Vaca")] -}

{- 7) Funcion tipo UNZIP -}

apellidos :: [(String, String, Int)] -> [String]
apellidos [] = []
apellidos ((n, a, e):xs) = a : (apellidos xs)

{- COMO TOMAR LOS APELLIDOS

Si esta vacia, devolve la lista vacia
Si no, agarra la primera terna, que es de la forma (n, a, e). Meto el a en la lista de apellidos que quedan.

[("Fran", "Trucco", 24), ("Dani", "Bravo", 20), ("Valen", "Vispo", 18)] -}

{- ("Daniel", "Bravo", 20),("Valentina","Vispo",18),("Rober", "Vaca", 100) -> ["Bravo","Vispo",Vaca"] -}

{- 8) -}

{- 9) -}

maximo1 :: [Int] -> Int
maximo1 [] = minBound
maximo1 (x:xs) = x `max` (maximo1 xs) 

sumaPares2 :: [(Int, Int)] -> Int
sumaPares2 [] = 0
sumaPares2 ((x,y):xs) = (x+y) + (sumaPares2 xs) 

todos0y1 :: [Int] -> Bool
todos0y1 [] = True
todos0y1 (x:xs) | (x==0 || x==1) = todos0y1 xs  
		| (x/=1 && x/=0) = False
		
quitar0s :: [Int] -> [Int]
quitar0s [] = []
quitar0s (x:xs) | x==0 = (quitar0s xs)
		| x/=0 = x: (quitar0s xs) 
{-Dani -}
{- ultimo:: [a]->a
ultimo []=[]
ultimo (x:xs)x:(ultimo xs)=ultimo xs -}

repetir :: Int -> Int -> [Int]
repetir 0 k = []  
repetir n k = k:(repetir (n-1) k) 
{- n>=0 -> k "arbitrario" -> [donde k aparece repetido n veces] -}
 
