{- ////////////////////////////////////////////// -}
                {- GUIA 2 -}
    {- Autor: Valentina S. Vispo - 2019 -}
{- ////////////////////////////////////////////// -}

exponente :: Int -> Int
exponente x = x^x

pitagoras :: Int -> Int -> Int
pitagoras x y = x^2+y^2

{- ////////////////////////////////////////////// -}

signo :: Float -> Int 
signo x | x>0 = 1
        | x==0 = 0
        | x<0 = -1

{- ////////////////////////////////////////////// -}

{- ////////////////////////////////////////////// -}

{- Guia 1 Ejercicio 19) Definı las funciones que 
describimos a continuacion, luego implementalas 
en haskell. 
Por ejemplo:
Enunciado: signo : Int → Int, que dado 
un entero retorna su signo, de la siguiente forma: 
retorna 1 si
x es positivo, -1 si es negativo y 0 en 
cualquier otro caso -}

{- a) entre0y9 : Int → Bool, que dado un entero 
devuelve True si el entero se encuentra entre 0 y 9 -}

entre0y9 :: Int -> Bool
entre0y9 x= 0 < x && x < 9

{- b) rangoPrecio : Int → String, que dado un numero 
que representa el precio de una computadora,
retorne “muy barato” si el precio es menor a 2000, 
“demasiado caro” si el precio es mayor que 5000,
“hay que verlo bien” si el precio est´a entre 2000 
y 5000, y “esto no puede ser!” si el precio es 
negativo -}

{- c) absoluto : Int → Int, que dado un entero 
retorne
su valor absoluto -}

{- d) esMultiplo2 : Int → Bool, que dado un entero n 
devuelve True si n es m´ultiplo de 2.
Ayuda: usar 
mod, el operador que devuelve el resto de la 
division -}

{- ////////////////////////////////////////////// -}

{- ////////////////////////////////////////////// -}

{- Guia 1 Ejercicio 22 -}

dispersion :: Float -> Float -> Float -> Float 
dispersion x y z | (((x>y)&&(x>z)) && (z>y))= x- y
                 | (((x>z)&&(x>y)) && (y>z))= x- z
                 | (((y>x)&&(y>z)) && (z>x))= y- x
                 | (((y>z)&&(y>x)) && (x>z))= y- z
                 | (((z>x)&&(z>y)) && (y>x))= z- x
                 | (((z>y)&&(z>x)) && (x>y))= z- y

{- ////////////////////////////////////////////// -}

{- Guia 1 Ejercicio 23: Definir la funcion 
celsiusToFahr:Num→Num, pasa una temperatura 
en grados Celsius a grados Fah-renheit. 
Para realizar la conversion hay que multiplicar 
por 1.8 y sumar 32 -}

celsiusToFahr :: Float -> Float
celsiusToFahr celsius = ((celsius*1.8)+32)

{- ////////////////////////////////////////////// -}

{- Guia 1 Ejercicio 24: Definir la funcion 
fahrToCelsius:Num→Num, la inversa de la anterior. 
Para realizar la conversion hay que primero restar 
32 y despues dividir por 1.8 -}

fahrToCelsius :: Float -> Float
fahrToCelsius fahr = ((fahr-32)/1.8)

{- ////////////////////////////////////////////// -}

{- Guia 1 Ejercicio 25: Definir la funcion 
haceFrioF:Num→Bool, indica si una temperatura 
expresada en grados Fahrenheit es frıa. 
Decimos que hace frıo si la temperatura es menor a 8
grados Celsius -}

haceFrioF :: Float -> Bool 
haceFrioF x |((fahrToCelsius x)<8) = True {-Frio Ejemplo: 45 -} 
            |((fahrToCelsius x)>8) = False {-Calor Ejemplo: 77 -}

{- ////////////////////////////////////////////// -}
