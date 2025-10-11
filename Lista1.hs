--Eduardo Pasqualeto

--exercicio1
ehTriangulo a b c = a + b > c && a + c > b && b + c > a

--exercicio2
tipoTriangulo a b c
    |(a == b && b == c) = "equilatero"
    |(a == b || a==c || b==c) = "isoceles"
    |(a /= b && b /= c && c /= a) = "escaleno"

--exercicio3
triangulo a b c 
    | ehTriangulo a b c = tipoTriangulo a b c
    | otherwise = "nao eh um triangulo"

--exercicio4
somaPares 0 = 0
somaPares 1 = 0
somaPares n
    | n `rem` 2 == 0 = n + somaPares (n-2)
    | otherwise = (n - 1) + somaPares (n-2) 

--exercicio5
somaPot2m m 0 = m
somaPot2m m n = (2^n) * m + somaPot2m m (n-1)

--exercicio6
primo n | n <= 1 = False
        | otherwise = primoAux n (n-1) --(n-1)=m

primoAux n 1 = True
primoAux n m
    |n `mod` m == 0 = False
    |otherwise = primoAux n (m-1)

--exercicio7 
seriePI n = seriePIAux n 0

seriePIAux n m 
    | abs termo <= 4 / fromIntegral n = 0 
    | otherwise = termo + seriePIAux n (m+1) 
        where termo = 4 * ((-1)^m) / fromIntegral (2*m + 1)

