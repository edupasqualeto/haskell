import Data.Char (isUpper, toLower)
--Eduardo Pasqualeto

--exercicio 1
pertence n [] = False
pertence n (a:ab)
    | n == a = True
    | otherwise = pertence n ab

--exercicio 2
intersecao [] ys = []
intersecao (x:xs) ys
    | x `elem` ys = x :intersecao xs ys
    | otherwise = intersecao xs ys

--exercicio 3
inverso [] = []
inverso (x:xs) = inverso xs ++ [x]

--exercicio 4
nUltimos n (x:xs)
    |n == length (x:xs) = (x:xs)
    |otherwise = nUltimos n xs

--ou

nPrim 0 _ = []
nPrim n (x:xs) = x : nPrim (n-1) xs
nUltimos2 n (x:xs) = inverso(nPrim n (inverso (x:xs)))

--exercicio 5
soma2 [] [] = []
soma2 _ [] = []
soma2 [] _ = []
soma2 (x:xs) (y:ys) = x+y : soma2 xs ys

--exercicio 6
pot2' 1 = [2]
pot2' n = 2^n : pot2' (n-1)
pot2 n = inverso (pot2' n)

--exercicio 7
intercalacao xs [] = xs
intercalacao [] ys = ys
intercalacao (x:xs) (y:ys)  | x > y = y : intercalacao ys (x:xs)
                            | otherwise = x: intercalacao xs (y:ys)

--exercicio 8
menor [x] = x
menor (x:xs) 
    | x < menor xs = x
    | otherwise = menor xs

--exercicio 9
removerElem 0 (x:xs) = x:xs
removerElem _ [] = [] 
removerElem n (x:xs)   
    | x == n = xs
    | otherwise = x : removerElem n xs

--exercicio 10
ordenar [] = []
ordenar xs =
    menor xs: 
    ordenar (removerElem (menor xs) xs)

--exercicio 11
insereOrd n (xs) = ordenar (n : xs)

--exercicio 12
enesimo n (xs) = xs !! (n-1)

--exercicio 13
repetir 0 _ = []
repetir n e = e : repetir (n-1) e

--exercicio 14
removeTab [] = []
removeTab (x:xs)    | x == '\t' = ' ' : removeTab xs 
                    | otherwise = x : removeTab xs

--exercicio 15 importado la no comeco
minusculas [] = []
minusculas (x:xs)   | isUpper x = toLower x : minusculas xs
                    | otherwise = x : minusculas xs

--exercicio 16
inversoDupla [] = []
inversoDupla ((x,y):xs) = (y,x) : inversoDupla xs 

--exercicio 17
simetrico [] = []
simetrico ((x,y):xs)    
    | x == y = True : simetrico xs
    | otherwise = False : simetrico xs

--exercicio 18
unidade 0 = '0'
unidade 1 = '1'
unidade 2 = '2'
unidade 3 = '3'
unidade 4 = '4'
unidade 5 = '5'
unidade 6 = '6'
unidade 7 = '7'
unidade 8 = '8'
unidade 9 = '9'
numString 0 = []
numString x = numString (div x 10) ++ [unidade (mod x 10)]   

--exercicio 19
unidade2 '0' = 0
unidade2 '1' = 1
unidade2 '2' = 2
unidade2 '3' = 3
unidade2 '4' = 4
unidade2 '5' = 5
unidade2 '6' = 6
unidade2 '7' = 7
unidade2 '8' = 8
unidade2 '9' = 9
stringNum [] = 0
stringNum (x:xs) = (unidade2 x) * (10^(length xs)) + stringNum xs

--exercicio20
decBin x 
    | div x 2 < 2 = [unidade (div x 2)] ++ [unidade (mod x 2)]
    | otherwise = decBin (div x 2) ++ [unidade (mod x 2)]

--exercicio21
binDec [] = 0
binDec (x:xs) = (unidade2 x) * (2^(length xs)) + binDec xs
    
--exercicio22
trocoCafe x y = trocoCafe2 (y-x) --(y-x) = z
trocoCafe2 0 = []
trocoCafe2 z 
    | z >= 50 = [(50, (div z 50))] ++ trocoCafe2 (mod z 50)
    | z < 50 && z >= 20 = [(20, (div z 20))] ++ trocoCafe2 (mod z 20)
    | z < 20 && z >= 10 = [(10, (div z 10))] ++ trocoCafe2 (mod z 10)
    | otherwise = [(5, (div z 5))]