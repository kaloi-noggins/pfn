nroParaDesc 0 = "zero"
nroParaDesc 1 = "um"
nroParaDesc 2 = "dois"
nroParaDesc _ = "JOOOJ"

fat 0 = 1
fat n = n * fat(n-1)

somatorio 0 = 0
somatorio n = n + somatorio(n-1) 

pList [] = "Lista Vazia"
pList (x:[]) = "Lista com um elemento: \n Elemento: "++show x
pList _ = "Lista com mais de uma coisa"