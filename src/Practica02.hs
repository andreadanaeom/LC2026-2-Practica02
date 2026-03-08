module Practica02 where

--Sintaxis de la logica proposicional
data Prop = Var String | Cons Bool | Not Prop
            | And Prop Prop | Or Prop Prop
            | Impl Prop Prop | Syss Prop Prop
            deriving (Eq)

instance Show Prop where 
                    show (Cons True) = "⊤"
                    show (Cons False) = "⊥"
                    show (Var p) = p
                    show (Not p) = "¬" ++ show p
                    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
                    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
                    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
                    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"


p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"

type Estado = [String]

--EJERCICIOS

--Ejercicio 1
variables :: Prop -> [String]
variables p = eliminarRepetidos (var p)
    where
        var (Var p) = [p]
        var (Cons _) = []
        var (Not p) = var p
        var (And p q) = var p ++ var q
        var (Or p q) = var p ++ var q
        var (Impl p q) = var p ++ var q
        var (Syss p q) = var p ++ var q

--Funcion auxiliar para la funcion variables, nos ayuda a construir una lista sin elementos repetidos.
eliminarRepetidos :: [String] -> [String]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) = x : eliminarRepetidos (filtrarRepetidos x xs)

--Funcion auxiliar que eliminar los repetidos a partir de comparar con un elemento
filtrarRepetidos :: String -> [String] -> [String]
filtrarRepetidos x [] = []
filtrarRepetidos x (y:ys) 
    | x == y = filtrarRepetidos x ys
    | otherwise = y : filtrarRepetidos x ys

--Ejercicio 2
interpretacion :: Prop -> Estado -> Bool
interpretacion (Var p) xs = pertenece p xs
interpretacion (Cons p) _ = p
interpretacion (Not p) xs = not (interpretacion p xs)
interpretacion (And p q) xs = interpretacion p xs && interpretacion q xs
interpretacion (Or p q) xs = interpretacion p xs || interpretacion q xs
interpretacion (Impl p q) xs = not (interpretacion p xs) || interpretacion q xs
interpretacion (Syss p q) xs = interpretacion p xs == interpretacion q xs

--Funcion auxiliar de la funcion interpretacion que nos dice si una variable pertenece a estado
pertenece :: String -> [String] -> Bool
pertenece _ [] = False
pertenece x (y:ys) 
    | x==y = True
    | otherwise = pertenece x ys

--Ejercicio 3
estadosPosibles :: Prop -> [Estado]
estadosPosibles p = conjPotencia (variables p)

--Ejercicio 4
modelos :: Prop -> [Estado]
modelos p = [estado | estado <- estadosPosibles p, interpretacion p estado]

--Ejercicio 5
sonEquivalentes :: Prop -> Prop -> Bool
sonEquivalentes p q = undefined

--Ejercicio 6 
tautologia :: Prop -> Bool
tautologia p = verificarTautologia p (estadosPosibles p) 
    where
        verificarTautologia _ [] = True
        verificarTautologia p (x:xs) = interpretacion p x && verificarTautologia p xs

--Ejercicio 7
contradiccion :: Prop -> Bool
contradiccion p = verificarContradiccion p (estadosPosibles p)
    where
        verificarContradiccion _ [] = True
        verificarContradiccion p (x:xs) = not (interpretacion p x) && verificarContradiccion p xs

--Ejercicio 8
consecuenciaLogica :: [Prop] -> Prop -> Bool
consecuenciaLogica = undefined


--Funcion auxiliar
conjPotencia :: [a] -> [[a]]
conjPotencia [] = [[]]
conjPotencia (x:xs) = [(x:ys) | ys <- conjPotencia xs] ++ conjPotencia xs