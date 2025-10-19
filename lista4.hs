--Questao 1
mapear f [] = []
mapear f (x:xs) = f x : mapear f xs

filtrar _ [] = []
filtrar p (x:xs)
    | p x = x : filtrar p xs
    | otherwise = filtrar p xs

reduzir f aux [] = aux
reduzir f aux (x:xs) = f x (reduzir f aux xs)

--Questao 2
quadrado xs = mapear (\x -> x^2) xs
resultadoq = quadrado [1..10]

--Questao 3
raiz xs = mapear (\x -> sqrt x) xs
resultador = raiz [4,9,16,25,36]

--Questao 4
somapares xs = reduzir (+) 0 (somaparesfilt xs)
somaparesfilt xs = filtrar (\x -> x `mod` 2 == 0) xs
resultadop = somapares [1..10]

--Questao 5
primo n | n <= 1 = False
        | otherwise = primoAux n (n-1) --(n-1)=m

primoAux n 1 = True
primoAux n m
    |n `mod` m == 0 = False
    |otherwise = primoAux n (m-1)

filtraPrimos xs = filtrar (\x -> primo x ) xs
resultadofp = filtraPrimos [2..8]

--Questao 6
