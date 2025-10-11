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
numString x | mod x 10 = div(mod x 10) 10 ++ [unidade x]   