--Estructuras Discretas 2017-2
--Profesor: Laura Freidberg Gojman
--Ayudante: Ricardo Jiménez Méndez
--Práctica 1
--Alumno:Jorge Ivan Perez Perez
--No. de Cuenta: 314211349

module Practica1 where

--Calcula el área de un círculo
areaCirculo :: (Num a,Fractional a) => a -> a
areaCirculo x = 3.1416*x^2

--Calcula el área de un triángulo
areaTri :: (Num a,Fractional a) => a -> a -> a
areaTri x y = (x*y)/2

--Calcula el volumen de una esfera
volEsfera :: (Num a,Fractional a) => a -> a
volEsfera x = (4*3.1416*x^3)/3

--Calcula la distancia entre dos puntos
distancia :: (Num a,Floating a) => (a,a) -> (a,a) -> a
distancia (x1,y1) (x2,y2) = sqrt((x2-x1)^2 + (y2-y1)^2)

--La primera entrada de cada tripleta se suma
--La segunda entrada de cada tripleta se multiplica
--La tercera entrada de cada tripleta se resta
opTrip :: (String, Int, String) -> (String,String,Int) -> (Int,String,String) -> (String,String,String)
opTrip (x,y,z) (a,b,c) (e,f,g) = (show (read x + read a + e), show (y * read b * read f), show (read z - c - read g))  

--Realiza la operación lógica implicación
implica :: Bool -> Bool -> Bool
implica True False = False
implica _ _ = True

--Realiza la operación lógica si y sólo si
syss :: Bool -> Bool -> Bool
syss a b = if a == b
          then True
          else False

--Dada una altura decir como es la persona
altura :: Float -> String
altura x
  | x >= 0.50 && x <= 1.20 = "Pitufo"
  | x >= 1.21 && x <= 1.50 = "Hobbit"
  | x >= 1.51 && x <= 1.65 = "Chaparro"
  | x >= 1.66 && x <= 1.75 = "Normal"
  | x >= 1.76 && x <= 1.90 = "Avatar"
  | otherwise = "Syntax ERROR :v"

--Nos dice si el primero es mayor o igual que el segundo
mayorIgual :: (Num a, Ord a) => a -> a -> Bool
mayorIgual x y = if x >= y
          then True
          else False

--Nos dice si los tres numeros son iguales
igualTres :: (Num a, Ord a) => a -> a -> a -> Bool
igualTres x y z = if x == y && y == z && x==z
          then True
          else False


{-- Pruebas --}

--Debe regresar 88.247544
prueba1 = areaCirculo 5.3

--Debe regresar 11.25
prueba2 = areaTri 3 7.5

--Debe regresar 212.1752864
prueba3 = volEsfera 3.7

--Debe regresar 2.3537204591879637
prueba4 = distancia (5,6.7) (4.5,9)

--Debe regresar ("15","432","-15")
prueba5 = opTrip ("3",9,"5") ("7","6",7) (5,"8","13")

--Debe regresar True
prueba6 = implica False True

--Debe regresar False
prueba7 = syss True False

--Aquí regresa el mensaje que pusiste
prueba8 = altura 1.89

--Debe regresar False
prueba9 = mayorIgual 8.6 12.5

--Debe regresar True
prueba10 = igualTres 8.5 8.5 8.5
