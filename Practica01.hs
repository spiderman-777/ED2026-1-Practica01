--Practica 01
--Funciones :
--1. Valor Abs :: Int -> Int
--La funcion valorAbs calcula el valor absoluto haciendo uso de if_then_else
valorAbs :: Int -> Int
valorAbs x = if x < 0 then -x else x 
-- Devuelve el valor absoluto de x, si es negativo se multiplica por -1
--2. esDivisor :: Int -> Int -> Bool 
--La funcion esDivisor devuelve True si el primer numero es divisor del segundo, False en caso contrario   
esDivisor :: Int -> Int -> Bool
esDivisor x y = y `mod` x == 0 
-- Devuelve True si el primero es divisor del segundo; False si no lo es
--3. cuadratica :: Float -> Float -> Float -> Float -> Float
--La funcion cuadratica calcula la evaluacion de la ecuacion a*x^2+b*x+c en v 
cuadratica :: Float -> Float -> Float -> Float -> Float
cuadratica a b c v = a * v^2 + b * v + c -- Calcula el valor de la ecuacion cuadratica en v
--4. sumaFracciones :: (Int, Int) -> (Int, Int) -> (Int, Int)
--La funcion sumaFracciones calcula la suma de dos fracciones 
--devuelve el resultado como una tupla con el resultado de sumar las fracciones que recibe como parametros
sumaFracciones :: (Int, Int) -> (Int, Int) -> (Int, Int)
sumaFracciones (a, b) (c, d) = 
    if b==d
    then (a + c, b)
    else (a * d + b * c, b * d)
-- Devuelve la suma de las fracciones a/b + c/d como una tupla (numerador, denominador)
--5. comparador :: Float -> Float -> Int
--La funcion comparador recibe 2 numeros n,m
--Devuelve 0 si n = m, 1 si n > m y -1 si m > n
comparador :: Float -> Float -> Int
comparador n m = 
    if n == m
    then 0
    else if n > m
         then 1
         else -1
-- Devuelve 0 si n=m; 1 si n>m; -1 si m>n
--6. puntoMedio :: (Float, Float) -> (Float, Float) -> (Float, Float)
--La funcion puntoMedio recibe 2 puntos del plano cartesiano
--Devuelve el punto medio entre ambos puntos
puntoMedio :: (Float, Float) -> (Float, Float) -> (Float, Float)
puntoMedio (x1, y1) (x2, y2) = ((x1 + x2) / 2, (y1 + y2)  / 2)
-- Devuelve el punto medio entre (x1,y1) y (x2,y2)
