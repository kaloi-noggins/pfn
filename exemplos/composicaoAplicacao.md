 # Aplicação ($) 

Altera a precedência dos operadores

## Exemplos
- f(g(z(x))) <=> f $ g & z x
- sqrt (3+9+2) <=> sqrt $ 3+9+2
- sum (map sqrt [1..100]) <=> sum $ map sqrt [1..100]
- sum (filter (>10) (map (*2) [2..10])) <=> sum $ filter (>10) $ map (*2) [2..10]

# Composição (.)

Compoem funções

## Exemplos
- f.g = \x -> f (g x)
- map (\xs a -> negate(sum(tail a))) [[1..5],[3..6],[1..7]] <=> map (negate.sum.tail) [[1..5],[3..6],[1..7]]
- oddSquareSum limit = sum (takeWhile (<=limit) (filter odd (map (^2) [1..]))) <=> oddSquareSum limit = sum . takeWhile(<=limit) . filter odd . map (^2) $ [1..]