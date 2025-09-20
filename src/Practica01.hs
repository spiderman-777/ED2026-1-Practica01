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
--Use la funcion auxiliar 'mod´ que calcula el resto de la division entera
-- Devuelve True si el primero es divisor del segundo; False si no lo es
--3. cuadratica :: Float -> Float -> Float -> Float -> Float
--La funcion cuadratica calcula la evaluacion de la ecuacion a*x^2+b*x+c en v 
cuadratica :: Float -> Float -> Float -> Float -> Float
cuadratica a b c v = a * v^2 + b * v + c 
-- Calcula el valor de la ecuacion cuadratica en v
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
puntoMedio :: (Float, Float) -> (Float, Float) -> (Float, Float)
puntoMedio (x1, y1) (x2, y2) = ((x1 + x2) / 2, (y1 + y2)  / 2)
-- Devuelve el punto medio entre (x1,y1) y (x2,y2)
--Relaciones
--Sean A y B conjuntos tales que A = B = {1,2,3,...,30}
--1. relacionDivisor :: Rel Int Int 
--En esta relacion R1, tenemos que aR1b si a y b tienen la misma paridad y a es divisor de b
--La funcion relacionDivisor admite dos numeros enteros a y b si cumplen las 2 condiciones, devuelve True en caso contrario False
--Primero definimos el tipo Rel para las relaciones entre dos conjuntos
type Rel a b = a -> b -> (a, b)
--La funcion devuelve la R1 sabiendo que aR1b si a y b tienen la misma paridad y a es divisor de b 
relacionDivisor :: Rel Int Int
relacionDivisor a b
  | even a == even b && b `mod` a == 0 = (a, b)
--funcion auxiliar even la utilice para verificar si los numeros son pares
--2. relacionSumaEspecial :: Rel Int Int
--La funcion devuelve la R2 sabiendo que aR2b si a + b es multiplo de 5 y a < b
relacionSumaEspecial :: Rel Int Int
relacionSumaEspecial a b
  | (a + b) `mod` 5 == 0 && a < b = (a, b)
--funcion auxiliar even la utilice para verificar si los numeros son pares
--3. relacionCongruentesModulo n :: Int -> Rel Int Int
--La funcion devuelve la R3 sabiendo que aR3b  se debe recibir un entero n y tenemos que aR3b si a %n = b %n con a̸ = b
relacionCongruentesModuloN :: Int -> Rel Int Int
relacionCongruentesModuloN n a b
  | a `mod` n == b `mod` n && a /= b = (a, b)
  

--NATURALES
-- Cero es natural, Suc Cero es natural, Suc Suc Cero es natural, etc.
data Natural = Cero | Suc Natural deriving (Show,Eq) --Esto es para que se muestre y que se puedan comparar

esPar :: Natural -> Bool
esPar = undefined

iguales :: Natural -> Natural -> Bool
iguales = undefined

maximo :: Natural -> Natural -> Natural 
maximo = undefined

potencia :: Natural -> Natural -> Natural
potencia = undefined

multiplicacion :: Natural -> Natural -> Natural
multiplicacion = undefined

suma :: Natural -> Natural -> Natural
suma = undefined
