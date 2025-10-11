f x y z
    | (x==7) = 10
    | (y==8) = 20
    | (z==9) = 30
    | otherwise = 30

-----

lucky 7 = "Numero da Sorte SETE!"
lucky x = "Desculpe, voce esta sem sorte, amigo!"

--------

libertagre 3 = "Voce esta correto!"
libertagre x = "voce nao sabe sobre futebol"

--------------

a :: Int -> Double
a 1 = sqrt 6
a p  = sqrt (6 + a(p-1))

soma :: Int -> Double
soma n = sum [a p | p <- [1..n]]

------------

seriePotencias _ 0 = 1
seriePotencias x n = x^n + seriePotencias x (n-1)  

------

somaDigitos n = somaDigitosAux n 0

somaDigitosAux n m 
    |n == 0 = m
    |otherwise = somaDigitosAux (n `div` 10) (m + (n `mod` 10))

-------

seriePi n = seriePiAux n 0 

seriePiAux n m
    | abs termo <= 4 / fromIntegral n = 0
    | otherwise = termo + seriePiAux n (m+1)
        where
            sinal = if even m then 1 else -1 --estava dando erro fazer o ((-1)^m), tive que pesquisar a solucao
            termo = 4 * fromIntegral sinal / fromIntegral (2*m + 1)

--------

remdupli [] =[]
remdupli (a:ab) | a `elem` ab = remdupli ab
                | otherwise = a: remdupli ab

-------------
fatiar _ _ [] = []
fatiar a b (x:xs)
    | (x !! a) && (x !! b)  = x : fatiar (a+1) b xs 
    | otherwise = fatiar a b xs