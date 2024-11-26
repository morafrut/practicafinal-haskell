data Var = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z deriving (Show, Eq, Ord)

data Formula = Atom Var
               |Neg Formula
               |Formula :&: Formula
               |Formula :|: Formula
               |Formula :=>: Formula
               |Formula :<=>: Formula deriving (Show, Eq, Ord)

infixl 9 :&:
infixl 9 :|:
infixl 7 :=>:
infixl 8 :<=>:

-------------------- EJERCICIO 1 --------------------
conjunto :: Eq a => [a] -> [a]
conjunto [] = []
conjunto (x:xs) = x : conjunto [y | y <- xs, y /= x]
variables :: Formula -> [Var]
variables (Atom var) = [var]
variables (Neg formula) = variables formula
variables (f1 :&: f2) = conjunto (variables f1 ++ variables f2)
variables (f1 :|: f2) = conjunto (variables f1 ++ variables f2)
variables (f1 :=>: f2) = conjunto (variables f1 ++ variables f2)
variables (f1 :<=>: f2) = conjunto (variables f1 ++ variables f2)

-----------------------------------------------------

-------------------- EJERCICIO 2 --------------------
negacion :: Formula -> Formula
negacion (Atom var) = Neg (Atom var)  
negacion (Neg formula) = formula    
negacion (f1 :&: f2) = Neg f1 :|: Neg f2  
negacion (f1 :|: f2) = Neg f1 :&: Neg f2
negacion (f1 :=>: f2) = f1 :&: Neg f2  
negacion (f1 :<=>: f2) = (Neg f1 :|: Neg f2) :&: (f1 :|: f2) 

-----------------------------------------------------

-------------------- EJERCICIO 3 --------------------
equivalencia :: Formula -> Formula
equivalencia (Atom var) = Atom var
equivalencia (Neg (Neg formula)) = equivalencia formula
equivalencia (Neg (f1 :&: f2)) = equivalencia (Neg f1 :|: Neg f2)
equivalencia (Neg (f1 :|: f2)) = equivalencia (Neg f1 :&: Neg f2)
equivalencia (Neg (f1 :=>: f2)) = equivalencia (f1 :&: Neg f2)
equivalencia (Neg (f1 :<=>: f2)) = equivalencia ((f1 :&: Neg f2) :|: (f2 :&: Neg f1))
equivalencia (Neg formula) = Neg (equivalencia formula)
equivalencia (f1 :&: f2) = equivalencia f1 :&: equivalencia f2
equivalencia (f1 :|: f2) = equivalencia f1 :|: equivalencia f2
equivalencia (f1 :=>: f2) = equivalencia (Neg f1 :|: f2)
equivalencia (f1 :<=>: f2) = equivalencia ((Neg f1 :|: f2) :&: (Neg f2 :|: f1))
-----------------------------------------------------

-------------------- EJERCICIO 4 --------------------
interpretacion :: Formula -> [(Var, Bool)] -> Bool
interpretacion (Atom var) valores = buscaValor var valores
interpretacion (Neg formula) valores = not (interpretacion formula valores)
interpretacion (f1 :&: f2) valores = (interpretacion f1 valores) && (interpretacion f2 valores)
interpretacion (f1 :|: f2) valores = (interpretacion f1 valores) || (interpretacion f2 valores)
interpretacion (f1 :=>: f2) valores = not (interpretacion f1 valores) || (interpretacion f2 valores)
interpretacion (f1 :<=>: f2) valores = (interpretacion f1 valores) == (interpretacion f2 valores)

buscaValor :: Var -> [(Var, Bool)] -> Bool
buscaValor var [] = error "No todas las variables están definidas."
buscaValor var ((v, valor):xs) =
  if var == v then
    valor
  else
    buscaValor var xs

-----------------------------------------------------

-------------------- EJERCICIO 5 --------------------
combinaciones :: Formula -> [[(Var, Bool)]]
combinaciones formula =
  generarComb (variables formula) []

generarComb :: [Var] -> [[(Var, Bool)]] -> [[(Var, Bool)]]
generarComb [] historial = historial
generarComb (x:xs) historial =
  [ (x, valor) : resto
  | valor <- [True, False],
    resto <- if null xs
             then [[]]
             else generarComb xs (historial ++ [[(x, valor)]])
  , not (elem ((x, valor) : resto) historial)
  ]

-----------------------------------------------------

-------------------- EJERCICIO 6 --------------------

tablaDeVerdad :: Formula -> [([(Var, Bool)], Bool)]
tablaDeVerdad formula =
  [ (comb, interpretacion formula comb)
  | comb <- combinaciones formula
  ]

-----------------------------------------------------
