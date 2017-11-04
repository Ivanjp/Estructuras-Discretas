module Practica6 where

{--
Estructuras Discretas 2018-1
Profesor: Laura Freidberg Gojman
Ayudante: Ricardo Jiménez Méndez
Práctica 6
Alumno:Perez Perez Jorge Ivan
No. de Cuenta:314211349
--}

--Tipo de dato para crear lógica proposicional
data Proposicion = Falso                                      -- False | 0
                 | Verdadero                                  -- True | 1
                 | Variable Var                               -- Cualquier letra del abecedario en mayúscula 
                 | Neg Proposicion                            -- ¬P 
                 | Con Proposicion Proposicion                -- P & Q
                 | Dis Proposicion Proposicion                -- P | Q
                 | Imp Proposicion Proposicion                -- P -> Q
                 | Syss Proposicion Proposicion deriving Eq   -- P <-> Q

--Tipo para definir variables en la lógica proposicional
type Var = Char

--Conjunto donde guardaremos el valor de verdad de las variables
--Si la variable se encuentra en el conjunto, su valor de verdad es True
--Si no, es False
type Estado = [Var]

--Lista por comprensión para tomar todas las letras del abecedario
abc = [x | x <- ['A'..'Z']]

--Nos regresa la variable que le pedimos del abecedario
tomaVar :: Var -> [Char] -> Var
tomaVar _ [] = error "no se encuentra el caracter"
tomaVar p (x:xs) = if (p == x)
                      then x
                      else tomaVar p xs

--Hacemos parte de la clase Show a Proposicion
--Para poder mostrar en pantalla de mejor manera las proposiciones
--Por ejemplo
-- Con (Neg (Variable (tomaVar 'P' abc))) (Variable (tomaVar 'Q' abc)) = (¬('P')) & ('Q')
instance Show Proposicion where
  show (Variable p) = show p
  show (Neg p) = "¬(" ++ show p ++ ")" 
  show (Con p q) = "(" ++ show p ++ "&" ++ show q ++ ")"
  show (Dis p q) = "(" ++ show p ++ "|" ++ show q ++ ")"
  show (Imp p q) = "(" ++ show p ++ "->" ++ show q ++ ")"
  show (Syss p q) = "(" ++ show p ++ "<->" ++ show q ++ ")"
  show Falso = "False"
  show Verdadero = "True"

--Dada una proposición y un estado, determinar el valor de verdad de la proposición
interpretacion :: Proposicion -> Estado -> Bool
interpretacion p est = case p of
       (Variable p) -> True
       (Neg p) -> True 
       (Con p q) -> True
       (Dis p q) -> True
       (Imp p q) -> True
       (Syss p q) -> False
--Regresa las variables de una proposición
--Si la variable se repite, solo debe de aparecer una vez
variables :: Proposicion -> [[Var]]
variables prop = case prop of
         (Variable p) -> [[p]]
         (Neg p) -> variables p
         (Con p q) -> variables p ++ variables q
         (Dis p q) -> variables p ++ variables q
         (Imp p q) -> variables p ++ variables q
         (Syss p q) -> variables p ++ variables q

{-- Pruebas --}
--Debe imprimir (('P') -> (¬('Q'))) <-> (('R') & (¬('S')))
prueba1 = Syss (Imp (Variable $ tomaVar 'P' abc) (Neg (Variable $ tomaVar 'Q' abc))) (Con (Variable $ tomaVar 'R' abc) (Neg (Variable $ tomaVar 'S' abc)))

--Debe regresar ["P","Q","R","S"]
prueba2 = variables prueba1

--Debe regresar False
prueba3 = interpretacion prueba1 ['P','S']

--Debe imprimir ((('P') -> ('Q')) & (('P') -> ('R'))) -> (('Q') -> ('R'))
prueba4 = Imp (Con (Imp (Variable $ tomaVar 'P' abc) (Variable $ tomaVar 'Q' abc)) (Imp (Variable $ tomaVar 'P' abc) (Variable $ tomaVar 'R' abc))) (Imp (Variable $ tomaVar 'Q' abc) (Variable $ tomaVar 'R' abc))

--Debe regresar ["P","Q","R"]
prueba5 = variables prueba4

--Debe regresar True
prueba6 = interpretacion prueba4 ['P']

--Debe imprimir ((¬('A')) | ('B')) -> (('C') & (¬('B')))
prueba7 = Imp ( Dis ( Neg (Variable $ tomaVar 'A' abc)) (Variable $ tomaVar 'B'abc))(Con ( Variable $ tomaVar 'C' abc) (Neg(Variable $ tomaVar 'B' abc)))
  
--Debe regresar ["A","B","C"]
prueba8 = variables prueba7

--Debe regresar True
prueba9 = interpretacion prueba7 ['C']

--Debe imprimir (((('R') & ('S')) -> ('T')) & (¬('T'))) <-> (('T') -> ('Q'))
prueba10 = Syss (Con(Imp (Con (Variable $ tomaVar 'R' abc) (Variable $ tomaVar 'S' abc))(Variable $ tomaVar 'T' abc))(Neg(Variable $ tomaVar 'T' abc)))(Imp(Variable $ tomaVar 'T' abc)(Variable $ tomaVar 'Q' abc))