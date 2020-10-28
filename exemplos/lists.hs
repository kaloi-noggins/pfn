listaA = [1..1125899906842624]

listaB = [2..50*2]

foo x = minimum x

ofo x = maximum x

oof listax n = take n (reverse listax)

menor [x] = x
menor [] = 0
menor (x:xs) = if x < menor xs then x else menor xs

maior [x] = x
maior []= 0
maior (x:xs) = if x > maior xs then x else maior xs