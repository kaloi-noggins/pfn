{-
retardimc p a
    | p / a ^ 2 <= 18.5 = "Garota de 14 anos"
    | p / a ^ 2 <= 25 = "OK"
    | p / a ^ 2 <= 30 = "Gorda"
    | otherwise = "U fuckin dying. Time to exercise"

imc1 dp a
    | imc <= 18.5 = "Garota de 14 anos"
    | imc <= 25 = "OK"
    | imc <= 30 = "Gorda"
    | otherwise = "U fuckin dying. Time to exercise"
    where imc = p / a^2
-}

imcs=[(95,1.85),(55,1.65),(120,1.85)]

imc' [] = []
imc' (x:xs) = imc:imc' xs
    where imc = (fst x)/((snd x)^2) 


get1 (a,b,c,d) = a
get2 (a,b,c,d) = b
get3 (a,b,c,d) = c
get4 (a,b,c,d) = d

notas = [(1,2,3,4),(2,3,4,5),(3,4,5,6),(4,5,6,7),(5,6,7,8),(6,7,8,9),(7,8,9,10)]

calcMedia [] = []
calcMedia (x:xs) = media : calcMedia xs
    where media = (get1 x + get2 x + get3 x + get4 x)/4    