cilindro r h=
    let al = 2*pi*r*h
        ab = pi*r^2
    in al * ab

somaPares [] = []
somaPares (x:xs)=
    let soma = fst(x) + snd(x)
    in soma:somaPares xs

somaPares' [] = []
somaPares' (x:xs)=soma:somaPares xs
    where soma = fst(x)+snd(x)

--aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa

contaElementos [] = []
contaElementos (x:xs)=
    let conta = (x, 1 + contaOcorrencia x xs)
        listaLimpa = removeRepetidos x xs
    in conta:contaElementos listaLimpa

contaOcorrencia _ [] = 0
contaOcorrencia n (x:xs)
    | n == x = 1 + contaOcorrencia n xs
    | otherwise = contaOcorrencia n xs

removeRepetidos _ [] = []
removeRepetidos n (x:xs)
    | n /= x = x:removeRepetidos n xs
    | otherwise = removeRepetidos n xs

--aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa

removeNaoPrimos [] = []
removeNaoPrimos (x:xs)
    | primo == True = x:removeNaoPrimos xs
    | otherwise = removeNaoPrimos xs 
    where primo = checaPrimalidade x x

checaPrimalidade _ 1 = True
checaPrimalidade n x
    | n==1 = False
    | x/=n && mod n x == 0 = False
    | otherwise = checaPrimalidade n (x-1)