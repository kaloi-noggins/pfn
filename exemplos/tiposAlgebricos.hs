convBool True = 1
convBool False = 0

data Semana = Segunda | Terca | Quarta | Quinta | Sexta | Sabado | Domingo

naoTrabson Sabado = True
naoTrabson Domingo = True
naoTrabson _ = False

data FormasGeo = Circulo Float Float Float| Retangulo Float Float Float Float

area :: FormasGeo -> Float
area (Circulo _ _ r) = pi * r
area (Retangulo x1 y1 x2 y2) = abs  (abs x2 - x1) * (abs y2 - y1)

data Mes = Jan | Fev | Mar | Abr | Mai | Jun | Jul | Set | Out | Nov | Dez deriving (Show, Eq)

ferias::Mes->Bool
ferias  Dez = True
ferias  Jan = True
ferias  Fev = True
ferias _ = False

data Pessoa = Pessoa String String Int deriving (Show,Eq)

nomePessoa::Pessoa -> String 
nomePessoa (Pessoa nome _ _ ) = nome

idadePessoa::Pessoa -> Int 
idadePessoa (Pessoa _ _ idade ) = idade

sobrenomePessoa::Pessoa -> String
sobrenomePessoa (Pessoa _ sobrenome _ ) = sobrenome

nomeCompletoPessoa::Pessoa -> String
nomeCompletoPessoa (Pessoa nome sobrenome _ ) = nome ++ " " ++ sobrenome

data Carro = Carro {
    marca::String,
    modelo::String,
    ano::Int
    } deriving (Show, Eq)

apresentaCarro::Carro->String
apresentaCarro (Carro {marca=mc, modelo=md, ano=a}) = "Carro "++md++" da marca "++mc++" do ano anofdsnãotaprintano"

uno = Carro "Fiat" "Uno" 2012
panzer = Carro
    {
        marca = "Volkswagen",
        modelo = "Panzer",
        ano = 1940
    }

type Nome = String
type Telefone = String    
type Agenda = [(Nome,Telefone)]

agenda::Agenda
agenda = [("Ana","11111111"),("Joooj","666666666")]