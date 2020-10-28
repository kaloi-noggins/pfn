{-Defnição de map-}

map' _ [] = []
map' f (x:xs) = f x : map' f xs

map'' f xs = [f x |x <- xs]

{-Definição de map com Foldl-}
map''' f xs = foldl (\acc x -> acc ++ [f x]) [] xs

{-Definição de map com Foldr-}
map'''' f xs = foldl (\acc x -> f x : acc) [] xs
{-=======================================-}

primeirosDupla xs = [fst x | x <- xs]

primeirosDupla' xs = map fst xs

{-=======================================-}

checaMaior x
    | fst x > snd x = fst x
    | otherwise = snd x

maiorDupla xs = map  checaMaior xs

{-=======================================-}

oof n m = map (*n) [1..m]

{-Definição de filter-}
filter' _ [] = []
filter' f (x:xs)
    | f x = x : (filter' f xs)
    | otherwise = filter' f xs

filter'' f xs = [x | x <- xs, f x]
{-=======================================-}

verificaPar xs = filter even xs

dobroPar xs = filter (>=20) (map (*2) xs)

{-exercícios-}

--ex 1
contaPalavras xs = zip xs (map length xs)

--ex 2
alunos xs = map getNome (filter getNota xs)
    where getNome(a,b,c) = b
          getNota(a,b,c) = c >= 7

{-
trd (a,b,c) = c
snd' (a,b,c) = b
alunos xs = [snd' x | x <- xs, trd x >= 7]

-}
--ex 3
somaQuadrados xs = sum (map (^2) xs)

--ex 4 
listaPositivo xs = map (>0) xs

--ex 5
capitalista n xs = map snd (filter (\x -> fst x <= n) (zip [1..] xs))

--ex 6
socialista n xs = map snd (filter (\x -> fst x >= n) (zip [1..] xs))

{-=======================================-}

somatorio n = foldr (+) 0 [1..n]
{-Definição de Fold-}

{-Foldl-}
foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f acc [] = acc
foldl' f acc (x:xs) = foldl' f (f acc x) xs

elem' y xs = foldl (\exp x -> if x == y then True else exp) False xs

{-Foldr-}
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f acc [] = acc
foldr' f acc (x:xs) = f x (foldr' f acc xs)

{-=======================================-}

{-exercícios-}
quicksort [] = []
quicksort (x:xs) = quicksort smol ++ [x] ++ quicksort bong
    where smol = filter (<=x) xs
          bong = filter (>x) xs


maximum' xs = last (quicksort xs)
maximum'' xs = foldl1 (\acc x -> if x>acc then x else acc) xs

--ex1
fatorial n = foldr (*) 1 [1..n]
fatorial' n = foldl (*) 1 [1..n]

--ex2
somaQua n = foldl (\acc x -> x^2 + acc) 0 [1..n] 

--ex3
tamanho xs = foldl (\acc x -> 1 + acc) 0 xs

--ex4
minimalista (x:xs) = foldl min x xs

--ex5
filter''' f = foldr (\x acc -> if f x then x:acc else acc) []
filter'''' f = foldl (\acc x -> if f x then acc ++ [x] else acc) []
