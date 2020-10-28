
f:: Int -> Int

f x=x

dobro x = 2*x

soma  x y = x+y

fat 0 = 1
fat n = n * fat(n - 1)

somadobro x y = dobro x + dobro y

somatorio 0 = 0
somatorio x = x + somatorio(x-1)

jooj x y = dobro x + (dobro y + y)

dobrofat x = if x<100 then 2*fat(x) else x
dobromaior x = if x>1000 then 2*x else x

owo x y = if mod x 2 == 0 then x + y else x-y

lenowo x y
    | x == 0 = 0
    | mod x 2 == 0 = x + y 
    | otherwise = x-y