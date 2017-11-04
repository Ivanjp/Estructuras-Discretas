--Estructuras Discretas 2018-1
--Profesor: Laura Freidberg Gojman
--Ayudante: Ricardo Jiménez Méndez
--Práctica 2
--Alumno:Perez^2 Jorge Ivan
--No. de Cuenta: 314211349

--Calcula el volumen de un cono
volCono :: (Num a,Floating a) => a -> a -> a
volCono x y = (3.1416*x^2*y)/3

--Calcula el area de un trapecio
areaTrap :: (Num a,Floating a) => a -> a -> a -> a
areaTrap bm b a = ((bm+b)/2)*a

--Te indica como eres dependiendo de tu edad
edad :: Int -> String
edad x
  | x >= 0 && x <= 3 = "Bebe"
  | x >= 4 && x <= 10 = "Niño"
  | x >= 11&& x <= 18 = "Adolescente"
  | x >= 19 && x <= 29 = "Joven"
  | x >= 30 && x <= 55 = "Chavo Ruco"
  | x >= 56 && x <= 76 = "Adulto"
  | x >= 77 && x <= 90 = "Anciano"
  | x >= 91 && x <= 110 = "Leyenda"
  | otherwise = "Syntax ERROR :v"

--Nos indica si un entero es par 
par :: Int -> Bool
par x = if mod x 2==0
          then True
          else False

--Nos indica si un entero es impar
impar :: Int -> Bool
impar y = if mod y 2==1
          then True
          else False

{--Pruebas--}

--Debe regresar 1413.72
prueba1 = volCono 10 (13.5)

--Debe regresar 76328.6288072
prueba2 = volCono (-45.7) (34.9)

--Debe regresar 1038.78
prueba3 = areaTrap (13) (46.7) (34.8)

--Debe regresar 568.26
prueba4 = areaTrap (56.2) (34) (12.6)

--Debe regresar el mensaje que pusiste
prueba5 = edad (-5)

--Debe regresar el mensaje que pusiste
prueba6 = edad 46

--Debe dar True
prueba7 = par 2358

--Debe dar False
prueba8 = par (-367)

--Debe dar True
prueba9 = impar (-8925)

--Debe dar False
prueba10 = impar 123456
