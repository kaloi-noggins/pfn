data List a = Empty | Cons a (List a) deriving Show

data Arvore a = Folha | No a (Arvore a) (Arvore a) deriving (Show,Ord,Eq)

--insereArvore :: a -> Arvore a -> Arvore a
insereArvore x Folha = No x (Folha) (Folha)
insereArvore x (No y arvoreEsquerda arvoreDireita)
    | x == y = No y arvoreEsquerda arvoreDireita
    | x < y = No y (insereArvore x arvoreEsquerda) arvoreDireita
    | x > y = No y arvoreEsquerda (insereArvore x arvoreDireita)

--encontraArvore :: a -> Arvore a -> Bool
encontraArvore x Folha = False
encontraArvore x (No y arvoreEsquerda arvoreDireita)
    | x == y = True
    | x < y = encontraArvore x arvoreEsquerda
    | x > y = encontraArvore x arvoreDireita


constroiArvore listaNos = foldr insereArvore Folha listaNos

--ex1
somaArvore Folha = 0
somaArvore (No x arvoreDireita arvoreEsquerda) = x + somaArvore arvoreEsquerda + somaArvore arvoreDireita

--ex2 
contaOcorrencia _ Folha = 0
contaOcorrencia x (No y arvoreDireita arvoreEsquerda)
    | x > y = contaOcorrencia x arvoreEsquerda
    | x < y = contaOcorrencia x arvoreDireita
    | otherwise = 1 + contaOcorrencia x arvoreDireita + contaOcorrencia x arvoreEsquerda