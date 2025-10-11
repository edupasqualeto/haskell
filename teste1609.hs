listat = [(x,y) | x<- [1,2,3], y<- ['a','b']]

somaesub (a,b) = (a+b,a-b)
somaesub2 (a,b) = a+b

lista2 = [(x,y,z) | x<-[1..10], y<-[1..10], z<-[1..10]]




notas [] = []
notas (x:xs) 
    | x >= 7 = ("Aprovado",x) : notas xs
    | otherwise = ("Reprovado",x) : notas xs

---exercicio 16
inversoDuplas [] = []
inversoDuplas ((x,y):xs) = (y,x) : inversoDuplas xs 

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
    

