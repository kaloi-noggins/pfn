--TODO 
    --Arrumar ex12
    --Fazer 9, 8, 14, 15

--ex1
fibonacci x
    | x == 0 = 0
    | x == 1 = 1
    | otherwise = fibonacci (x-1) + fibonacci (x-2)

--ex2
elemento [] _ = error "Posição maior que tamanho da lista"
elemento (x:xs) n
    | n == 0 = x
    | otherwise = elemento xs (n-1)

--ex3
pertence _ [] = False
pertence n (x:xs)
    | n == x = True
    | otherwise = pertence n xs

--ex4
contaOcorrencia _ [] = 0
contaOcorrencia n (x:xs)
    | n == x = 1 + contaOcorrencia n xs
    | otherwise = contaOcorrencia n xs

--ex5
unicaOcorrencia n [] = False
unicaOcorrencia n (x:xs) 
    | n == x = unicaOcorrencia' n xs
    | otherwise = unicaOcorrencia n xs

unicaOcorrencia' n [] = True
unicaOcorrencia' n (x:xs) 
    | n == x = False
    | otherwise = unicaOcorrencia' n xs


--ex6 
maioresQue _ [] = []
maioresQue n (x:xs)
    | x > n = x : maioresQue n xs
    | otherwise = maioresQue n xs

--ex7
remover _ [] = []
remover n (x:xs)
    | n == x = xs
    | otherwise = remover n xs

--ex8 
removerRepetidos [] = []
removerRepetidos (x:xs) = x:removerRepetidos' x xs

removerRepetidos' _ [] = []
removerRepetidos' n (x:xs)
    | n /= x = x:removerRepetidos' n xs
    | otherwise = removerRepetidos' n xs

--ex9


--ex10 
geraSequencia n = geraSequencia' n (n-1)

geraSequencia' n (-1) = []
geraSequencia' n m = n - m : -(n - m) : geraSequencia' n (m-1)


--ex11
intercala [] [] = []
intercala (x:xs) [] = x:intercala xs []
intercala [] (y:ys) = y:intercala [] ys
intercala (x:xs) (y:ys) = x:y:intercala xs ys

--ex12 -- Broken (repete elementos)
uniao [] [] = []
uniao (x:xs) [] = x : uniao xs []
uniao [] (y:ys) = y : uniao [] ys
uniao (x:xs) (y:ys)
    | x /= y = x : y : uniao xs ys
    | otherwise = uniao xs ys

--ex13
sequencia 0 _ = []
sequencia n m = m : sequencia (n-1) (m+1)

--ex14


--ex15


--ex16
palindrome [] = True
palindrome (x:xs)
    | xs==[] = True
    | x /= last xs = False
    | otherwise = palindrome (init xs)

--ex17
somaDigitos n
    | n == 0 = 0
    | otherwise = (mod n 10) + somaDigitos (div (n - (mod n 10)) 10)


--ex18
separaParImpar (x:xs) = (separaPar (x:xs), separImpar (x:xs))

separaPar [] = []
separaPar (x:xs)
    | mod x 2 == 0 = x: separaPar xs
    | otherwise = separaPar xs

separImpar [] = []
separImpar (x:xs)
    | mod x 2 /= 0 = x:separImpar xs
    | otherwise = separImpar xs