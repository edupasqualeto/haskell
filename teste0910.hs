import Data.Char

transforma f (x:xs) = f x : transforma f xs

maiusculas = toUpper
minusculas = toLower

numString 0 = "zero"
numString 1 = "um"
numString 2 = "dois"
numString 3 = "tres"
numString 4 = "quatro"
numString 5 = "cinco"
numString 6 = "seis"
numString 7 = "sete"
numString 8 = "oito"
numString 9 = "nove"

filtrar::(a -> Bool) -> [a] -> [a]
filtrar _ [] = []
filtrar p (x:xs) 
    |p x = x : filtrar p xs
    |otherwise = filtrar p xs

--filtrar (\x->mod x 2  == 0) [1,2,3,4,5,6] -- retorna so os pares

--filtrar (\x->length x < 5) ["haskell","tst"] -- retorna so as strings com menos que 5 letras

--filtrar (\x->last  x ==  'a') ["haskella","tst"] -- retorna so as strings que terminam em a