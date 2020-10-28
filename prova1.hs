{-
    Aluno: Kalyl Henings
    Palpite da nota: 8.0 
        Provavelmente errei a segunda por causa dos operadores. 
        Utilizei x>=y>=z, que não é suportado
-}

somaQuadrados x y z   
    | x >= y && x >= z && = x^2 + y^2
    | y >= x && x >= z = x^2 + z^2
    | otherwise = x^2 + z^2

{-========================================-}
unico [] = []
unico (x:xs)
    | unico' x xs == True = x:unico xs
    | otherwise = unico listaLimpa
    where listaLimpa = removeRepetidos x xs

unico' _ [] = True
unico' n (x:xs)
    | n == x = False
    | otherwise = unico' n xs

removeRepetidos _ [] = []
removeRepetidos n (x:xs)
    | n /= x = x:removeRepetidos n xs
    | otherwise = removeRepetidos n xs

{-========================================-}
inverte [] = []
inverte (x:xs) = (snd x,fst x) : inverte xs

{-========================================-}
simetrico []=[]
simetrico (x:xs)
    | fst x == snd x = True:simetrico xs
    | otherwise = False:simetrico xs