--1--
esCero::Int -> Bool
esCero x |x==0 = True
         |otherwise = False

esPositivo::Int -> Bool
esPositivo x | x>0 = True
             | x==0 = False
             | x<0 = False

esVocal:: Char -> Bool
esVocal n |(n=='a')||(n=='e')||(n=='i')||(n=='o')||(n=='u') = True
          | otherwise = False

--2--
paraTodo::[Bool] -> Bool
paraTodo [] = True
paraTodo (x:xs)| (x==True) = paraTodo xs
               | (x==False) = False

sumatoria::[Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

--[1[2[3[4[5[]]]]]] = [1,2,3,4,5]--

productoria::[Int] -> Int
productoria [] = 1
productoria (x:xs) = x * productoria xs

factorial:: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

promedio::[Int] -> Int
promedio [] = 0
promedio (x:xs) =  div (sumatoria (x:xs)) (length (x:xs))

--3--
pertenece::Int -> [Int] -> Bool
pertenece n [] = False
pertenece n (x:xs)| n==x = True
                  | n/=x = pertenece n xs

--4--
encuentra::Int -> [(Int, String)] -> String
encuentra n [] = ""
encuentra n ((x,y):xs)|n==x = y
                      |n/=x = encuentra n xs

--5--
paraTodo1::[a] -> (a -> Bool) -> Bool
paraTodo1 [] t = True
paraTodo1 (x:xs) t|(t x == True) = paraTodo1 xs t
                  |(t x == False) = False

existe1::[a] -> (a -> Bool) -> Bool
existe1 [] t = False
existe1 (x:xs) t|(t x == True) = True
                |(t x == False) = existe1 xs t

sumatoria1:: [a] -> (a-> Int) -> Int
sumatoria1 [] t = 0
sumatoria1 (x:xs) t = t x + sumatoria1 xs t

productoria1::[a] -> (a-> Int) -> Int
productoria1 [] t = 1
productoria1 (x:xs) t = t x * productoria1 xs t

--6--
paraTodo11:: [Bool] -> Bool
paraTodo11 xs = paraTodo1 xs id

--7--
todosPares:: [Int] -> Bool
todosPares []= False
todosPares (x:xs) = paraTodo1 (x:xs) even

hayMultiplo :: Int -> [Int] -> Bool
hayMultiplo n [] = False
hayMultiplo n xs = existe1 xs (\x -> (mod x n == 0))

sumaCuadrados:: Int -> Int
sumaCuadrados x = sumatoria1 [0..x] (^2)

factorial1:: Int -> Int
factorial1 x = productoria1 [x] factorial

multiplicaPares:: [Int] -> Int
multiplicaPares [] = 1
multiplicaPares xs = productoria1 (hacerXsPar xs) (*1)

--creamos 2 funciones que nos pueden servir para verificar que un num sea par y otra para obtener una lista de pares
esPar :: Int -> Bool
esPar n = mod n 2 == 0

hacerXsPar :: [Int] -> [Int]
hacerXsPar [] = []
hacerXsPar (x:xs) |mod x 2 == 0 = x: hacerXsPar xs
                  |mod x 2 /= 0 = hacerXsPar xs

--8--

{-
map: es la lista obtenida de aplicar una funcion a un lista
(a->b)->[a]->[b]
filter: es la lista de elementos de xs que verifica un predicado
(a->Bool)->[a] -> [a]

map succ [1,-4,6,2,-8]==[2,-3,7,3,-7]
filter esPositivo [1,-4,6,2,-8]==[1,6,2]
-}

--9--
dobleDeXs::[Int] -> [Int]
dobleDeXs [] = []
dobleDeXs (x:xs) = x*2 : dobleDeXs xs

dobleDeXs1:: [Int] -> [Int]
dobleDeXs1 xs = map (*2) xs

--10--
listaDePares::[Int] -> [Int]
listaDePares []=[]
listaDePares (x:xs)|mod x 2 == 0 = x: hacerXsPar xs
                   |mod x 2 /= 0 = hacerXsPar xs

listaDePares1::[Int] -> [Int]
listaDePares1 []=[]
listaDePares1 (x:xs) = filter (pares) (x:xs)

--creamos otra funcion para usarla con el filter--
pares::Int -> Bool
pares x|(mod x 2==0) = True
       |(mod x 2/=0) = False

multiplicaPares1::[Int] -> Int
multiplicaPares1 (x:xs)=productoria' (filter (pares) (x:xs)) (*1)

--11--
sumarALista::Num a => a -> [a] -> [a]
sumarALista n []=[]
sumarALista n (x:xs)= (n+x): sumarALista n xs

encabezar::a -> [[a]] -> [[a]]
encabezar n []=[]
encabezar n (x:xs)= (n:x): encabezar n xs

mayoresA:: Ord a => a -> [a] -> [a]
mayoresA n []= []
mayoresA n (x:xs)| (n < x)= x: mayoresA n xs
                 | (n>= x)= mayoresA n xs

sumarALista1:: Num a => a -> [a] -> [a]
sumarALista1 n (x:xs)= map (+n) (x:xs)

encabezar1::a -> [[a]] -> [[a]]
encabezar1 n xs= map (n:) xs

mayoresA1:: Ord a => a -> [a] -> [a]
mayoresA1 n xs= filter (n<) xs

--12--
--encuentra::Int -> [(Int, String)] -> String
--encuentra n [] = " "
--encuentra n ((x,y):xs)|n==x = y
--                      |n/=x = encuentra n xs

encuentra1::Int -> [(Int,String)] -> String
encuentra1 n ts = g (filter (esIgualNum n) ts)
                   where g [] = " "
                         g (t:ts1) = snd t

esIgualNum :: Int -> (Int,String) -> Bool
esIgualNum n (x,y) = (n==x)


--13--
primIgualesA:: Eq a => a -> [a] -> [a]
primIgualesA n []=[]
primIgualesA n (x:xs)|(n==x) = x: primIgualesA n xs
                     |(n/=x) = []

{- takeWhile: toma un predicado y una lista, y recorre la lista desde
el principio y devuelve estos elementos mientras el predicado
se mantenga cierto
takeWhile:: (a -> Bool) -> [a] -> [a]
-}

primIgualesA1::Eq a=> a ->[a] -> [a]
primIgualesA1 n xs = takeWhile (n==) xs

--14--
primIguales::Eq a =>[a] -> [a]
primIguales (x:xs)|(x==xs!!0)= x: primIguales xs
                  |(x/=xs!!0)= x:[]

primIguales1::Eq a => [a] -> [a]
primIguales1 (x:xs)=primIgualesA x (x:xs)

--15--
minimo::Ord a => [a] -> a
minimo [x] =x
minimo (xs) = minimum xs

minimo1::(Ord a, Bounded a) =>[a] -> a
minimo1 []= minBound
minimo1 [a]=a
minimo1 (x:xs) = minimum (x:xs)

{- case sintaxis
case (expresion) of <patron> -> <resultado>
-}