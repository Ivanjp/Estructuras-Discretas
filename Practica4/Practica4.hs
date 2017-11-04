module Practica4 where

--Estructuras Discretas 2018-1
--Profesor: Laura Freidberg Gojman
--Ayudante: Ricardo Jiménez Méndez
--Práctica 4
--Alumno: Pérez Pérez Jorge Iván
--No. de Cuenta: 314211349

--Dada una lista de enteros
--Regresar la suma de todos esos enteros
sumaNum :: [Int] -> Int
sumaNum [] = 0
sumaNum (x:xs) = x + sumaNum xs

--Dado un número n y una lista de numeros
--Regresa cuantas veces aparece n en la lista
cuentaNum :: (Eq a) => a -> [a] -> Int
cuentaNum r []= 0
cuentaNum r (x:xs) | x==r=1+(cuentaNum r (xs)) 
                   | otherwise = (cuentaNum r (xs))

--Dada una lista de elementos
--Regresa una lista que indica cuantas veces aparece cada elemento en la lista original
repeticiones :: (Num a,Eq a) => [a] -> [(a,Int)]
repeticiones [] = []
repeticiones (x:xs) = duplas(elimina (x:xs)) (x:xs) 

--Dados dos enteros n y m
--Regresar n elevado a la m
eleva :: Int -> Int -> Int
eleva n 0 = 1
eleva n m = n*(eleva n (m-1))

--Dada una lista de números
--Regresar el promedio de ella
promedio :: [Int] -> Int
promedio [] = 0
promedio p = div (sumaNum (p)) (long (p))

--Funciones complementaria para "promedio"---
long :: [a] -> Int
long [] = 0
long [x] = 1
long (x:xs) = 1 + long (xs)

--Funciones complementarias para "repeticiones"---

eliminaTodos :: (Num a,Eq a) => a -> [a] -> [a]
eliminaTodos _ [] = []
eliminaTodos e (x:xs)= if (x==e) 
                       then eliminaTodos e xs 
                       else [x] ++ eliminaTodos e xs
elimina :: (Num a, Eq a) => [a] -> [a]
elimina [] = []
elimina (x:xs) = [x] ++ elimina(eliminaTodos x xs)

duplas :: (Eq a) => [a] -> [a] -> [(a,Int)]
duplas _ [] = []
duplas [] _ = []
duplas (x:xs) (y:ys) = [(x, cuentaNum x (y:ys))] ++ duplas xs (y:ys) 

{--Pruebas--}

--Debe regresar 414
prueba1 = sumaNum [45,78,12,89,134,56]

--Debe regresar 4834
prueba2 = sumaNum [456,1234,789,2345,10]

--Debe regresar 6
prueba3 = cuentaNum 7 [7,3,5,7,12,7,89,7,0,7,23,7]

--Debe regresar 4
prueba4 = cuentaNum 34 [56,34,78,59,34,80,34,12,50,34]

--Debe regresar [(5,3),(7,3),(4,2),(3,1),(8,2),(1,2)]
prueba5 = repeticiones [5,7,5,4,3,4,7,7,8,5,8,1,1]

--Debe regresar [(45,4),(67,3),(23,1),(89,2),(24,1),(56,1),(90,2),(78,1),(34,1),(12,1)]
prueba6 = repeticiones [45,45,67,23,67,89,24,56,90,78,45,34,12,67,89,90,45]

--Debe regresar 262144
prueba7 = eleva 8 6

--Debe regresar 1048576
prueba8 = eleva 32 4

--Debe regresar 45
prueba9 = promedio [45,23,67,12,78,58,24,58]

--Debe regresar 4
prueba10 = promedio [2,4,7,4,8,6,9,3,5,2,7,1]
