--ex1
multiplica _ [] = []
multiplica n (x:xs) = (n*x) : multiplica n xs

--ex2
multi20 [] = []
multi20 (x:xs)
    | 2*x >= 20 = 2*x : multi20 xs
    | otherwise = multi20 xs

--ex3
inverte [] = []
inverte (x:xs) = inverte xs ++ [x]

--ex4
divisivel7 [] = []
divisivel7 (x:xs)
    | mod x 7 == 0 = x : divisivel7 xs
    | otherwise = divisivel7 xs

--ex5
oof [] = []
oof (x:xs) 
    | mod x 3 == 0 = "BOOM!" : oof xs
    | mod x 7 == 0 = "BANG!" : oof xs
    | otherwise = oof xs

--ex6
naodiv2 [] = []
naodiv2 (x:xs)
    | mod x 2 /= 0 = x : naodiv2 xs
    | otherwise = naodiv2 xs

--ex7
tamanho [] = 0
tamanho (x:xs) = 1 + tamanho xs

--ex8
soma [] = 0
soma (x:xs) = x + soma xs

--ex9
multilista [] [] = []
multilista (x:xs) [] = x : multilista xs []
multilista [] (y:ys) = y : multilista ys []
multilista (x:xs) (y:ys) = x*y : multilista xs ys

--ex10
maior [] [] = []
maior (x:xs) [] = x : maior xs []
maior [] (y:ys)= y : maior ys []
maior (x:xs) (y:ys) 
    | x>y = x : maior xs ys
    | otherwise = y : maior xs ys