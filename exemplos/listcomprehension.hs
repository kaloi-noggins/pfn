dobro n = [2*x|x<-[1..n]]

--Sintaxe Casamente de Padrões
dobro12' [] = []
dobro12' (x:xs)
    | x >= d12 = d12:dobro12' xs
    | otherwise = dobro12' xs
    where d12 = x*12

--Sintaxe Compreensão de Lista
dobro12 xs = [ 2*x | x <- xs , 2*x >= 12]

--aaaaaaaaaaaaaaaaaaa
valium xs a b= [x | x <- xs, x >= a, x <= b, mod x 7 == 0,mod x 3 == 0]

--aaaaaaaaaaaaaaaaaaa
multiLista xs ys = [x*y | x <- xs, y <-ys]

--aaaaaaaaaaaaaaaaaa
contaElementos xs = sum [1 | _ <-xs ]

--exercicios

-- ex 1
somaPares xs = [fst x + snd x | x <- xs]

--ex - concatenaLista : Recebe uma lista de listas e retorna uma única lista  2 Eu não sei como fazer

{-
concatenaLista [] = []
concatenaLista (x:xs) = x ++ concatenaLista xs
-}

concatenaLista :: [[a]] -> [a]
concatenaLista xss = [x | xs <- xss, x <- xs] 

concatenaLista' :: [[[a]]] -> [a]
--[[[1,2],[3]],[],[4,5]]
concatenaLista' xsss = [x | xss <- xsss, xs <-xss, x <- xs]

--ex3
capitaLista a xs = [x | x <- xs, x <= a]

--ex4
inverso xs = [(snd x, fst x) | x <- xs]

--ex5 
simetrico xs = [x | x <- xs , fst x == snd x]

--ex6
divisores n = [x | x <- [1..n] , mod n x == 0]

--ex7 
replicarLista xs = [replicate x x | x <- xs]

--ex8
contaOcorrencias xs n = sum [1 | x <- xs, x==n]