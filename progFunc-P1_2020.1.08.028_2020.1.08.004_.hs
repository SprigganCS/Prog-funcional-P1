import Data.Char
{------------------------------------------
Prova 01 Programação Funcional 07/07/2021
-------------------------------------------
Nome completo 01: André Neves Medeiros
Matricula 01: 2020.1.08.028

Nome completo 02: Arthur Barros Klimas
Matrícula 02: 2020.1.08.004

-------------------------------------------
Instruções para o preenchimento do script
a) Implemente o código na sequência em que aparece na prova
b) separe cada questão com as linhas pontilhadas (abaixo)
   {--Questão X -----------------------------------------------}
c) Questões com mais de um item, separe-os da seguinte forma:

{--Questão  ----------------------------------------------}

--Item X.a
 código da questão X item a

--Item X.b
  código da questão X item b

IMPORTANTE: Todo o script deve ter as citações de todas as questões e itens.

Então, as questões ou itens não solucionados por você deverão receber, impreterivelmente, o texto --EM BRANCO --
Caso você não organize a prova como indicado, poderá ser penalizado na nota final

d) Não use nomes de funções diferentes dos indicados nas questões.
De forma alternativa, e caso o nome não seja sugerido, dê preferência por começar como: funcQx, em que x refere-se à questão.
Ex: funcQ3a pode ser a implementação da questão 3, item a

e) não é necessário (mas não é um problema) incluir o enunciado das questões no script
f) Você pode inserir comentários pessoais explicando o código implementado antes do cabeçalho da função

g) QUESTÕES COM TÉCNICAS NÃO APRESENTADAS EM AULA NÃO SERÃO CONSIDERADAS


-------------------------------------------}

-- +++++++++++++++++ comece, aqui, sua prova. Boa prova +++++++++++++++++++++++ --

{----Questão 1 ------------------------------------------------------}

--Questão 1.a
funcQ1a :: Double -> Double
funcQ1a x
  |x<0 = (2/x)
  |x==0 = (x+4)/(x-2)
  |x>0 = (x+4)/(x-2)


--Questão 1.b
funcQ1b :: Int -> Int -> Int
funcQ1b x y
  |x<y = (x-y)
  |x>y = (x+y)
  |x==y = (x+y)


--Questão 1.c
funcQ1c :: Int -> Int -> Int -> Int
funcQ1c x y z
  |(x+y)>z = x+y+z
  |(x+y)<z = x-y-z
  |(x+y)==z = 0



{----Questão 2 ------------------------------------------------------}
{--o problema estava na ausência do fat 0--}
fat :: Int->Int
fat 0 = 1
fat x = x * fat(x-1)


{----Questão 3 ------------------------------------------------------}
soma::Int->Int->Int
soma a b = a+b

funcQ3::Int->Int->Int
funcQ3 _ 0 = 0
funcQ3 0 _ = 0
funcQ3 x y = soma x (funcQ3 x (y-1))


{----Questão 4 ------------------------------------------------------}
contaCasas::Int->Int
contaCasas x
	|x<10 = 0
	|otherwise = 1 + contaCasas (x `div` 10)

invertInt::Int->Int
invertInt y |y<10 = y
invertInt x = b*z+ (invertInt a)
	where
	a = x `div` 10
	b = x `mod` 10
	z = 10^contaCasas x


{----Questão 5 ------------------------------------------------------}
square::Int->Int
square x = x*x

fourPower::Int->Int
fourPower x = square x * square x


{----Questão 6 ------------------------------------------------------}
funcQ6::Int->Float
funcQ6 0 = sqrt 6
funcQ6 x = sqrt (6 + funcQ6 (x-1))


{----Questão 7 ------------------------------------------------------}
--considerando que não escolher também é uma maneira de escolher
funcQ7::Int->Int
funcQ7 0 = 1
funcQ7 x = fat x + funcQ7 (x-1)


{----Questão 8 ------------------------------------------------------}
--funciona mesmo que primeiro número seja menor que o segundo
funcQ8::Int->Int->Int
funcQ8 x y
	|x<y = funcQ8 y x
	|y==0 = x
	|otherwise = funcQ8 y (x `mod` y)


{----Questão 9 ------------------------------------------------------}
funcQ9 :: Int -> Int -> Int -> Int -> Int
funcQ9 x y z q
  |y > z = q
  |y `mod` x == 0 = funcQ9 x (y+1) z (q+1)
  |otherwise = funcQ9 x (y+1) z q

howManyMultiples :: Int -> Int -> Int -> Int
howManyMultiples x y z = funcQ9 x y z 0



{---- Questão 10 -----------------------------------------------------}
lastDigit :: Int -> Int
lastDigit x
    |(x<0) = ((-1) * x) `rem` 10
    |otherwise = x `rem` 10


{--Questão 11 ----------------------------------------------}
funcQ11lis :: Int -> [Int]
funcQ11lis 0 = []
funcQ11lis x = funcQ11lis (x `div` 10) ++ [x `mod` 10]

funcQ11Val :: Int -> [Int] -> Int
funcQ11Val y (a:x) | y > length (a:x) = -1
funcQ11Val 0 (a:x) = a
funcQ11Val n (a:x) = funcQ11Val (n-1) x

anyDigit :: Int -> Int -> Int
anyDigit m n = funcQ11Val m (funcQ11lis n)


{----Questão 12 ------------------------------------------------------}
-- Q12a: não há verificação entre m e p, podendo essas variáveis terem o mesmo valor


-- Q12b:
allDifferent::Int-> Int-> Int -> Bool
allDifferent m n p = (m/=n) && (n/=p) && (m/=p)


{---- Questão 13 -----------------------------------------------------}
howManyEqual::Int->Int->Int->Int
howManyEqual a b c
  |a==b && b==c = 3
  |a==b && b/=c = 2
  |a==c && a/=b = 2
  |b==c && a/=b = 2
  |otherwise = 0



{---- Questão 14 -----------------------------------------------------}
--tabela de vendas
sales ::Int->Int
sales 0 = 2
sales 1 = 41
sales 2 = 72
sales 3 = 48
sales 4 = 32
sales 5 = 0
sales 6 = 55
sales 7 = 4

periodo::Int
periodo = 7


--Q14a
howManyLess :: Int -> Int -> Int -> Int
howManyLess v b e
 | b > e || b > 7 = 0
 | v > sales b = 1 + howManyLess v (b+1) e
 | otherwise = howManyLess v (b+1) e


--Q14b
noZeroInPeriod :: Int -> Bool
noZeroInPeriod 0 = sales 0 /= 0
noZeroInPeriod d = sales d /= 0 && noZeroInPeriod (d - 1)
{--o int recebido é até que dia do mês irá checar ----}


--14c
zerosInPeriod :: Int -> [Int]
zerosInPeriod (-1) = []
zerosInPeriod l
    | sales l == 0 = l : zerosInPeriod (l-1)
    | otherwise = zerosInPeriod (l-1)

zerosInPeriodAux :: [Int]
zerosInPeriodAux = zerosInPeriod periodo


--14d {---Obs: especificar período, ex: funcQ14d valor periodo ---}
funcQ14d :: Int -> Int -> [Int]
funcQ14d _ (-1) = []
funcQ14d v d
 |v>(sales d)= d : funcQ14d v (d-1)
 |otherwise = funcQ14d v (d-1)



{--Questão 15 ----------------------------------------------}
funcQ15 :: Int -> Int
funcQ15 0 = 0
funcQ15 1 = 1
funcQ15 x = (funcQ15(x-1)+funcQ15(x-2))

fib :: Int -> Int -> Int
fib x y
  |y > x = -1
  |funcQ15 y == x = y
  |otherwise = fib x(y + 1)

antFib :: Int -> Int
antFib x = fib x 0



{--Questão 16 ----------------------------------------------}
funny :: Int -> Int -> Int -> Bool
funny  x y z = (x > z && y <= z)


{--Questão 17 ----------------------------------------------}
funcQ17 :: Char -> Char
funcQ17 x
  |isLower (x) == True  =  toUpper (x)
  |otherwise = x



{--Questão 18 ----------------------------------------------}
charToNum :: Char -> Int
charToNum x
  |isDigit (x)==True = digitToInt (x)
  |otherwise = -1


{--Questão 19 ----------------------------------------------}
duplicate::String->Int->String
duplicate _ 0 = ""
duplicate s n = s ++ pushRight s (n-1)


{--Questão 20 ----------------------------------------------}
tamanhoS::String->Int
tamanhoS (a:[]) = 2
tamanhoS (a:b) = 1+tamanhoS b

pushRight::String->Int->String
pushRight s n
	|n > tamanhoS s = ">" ++ pushRight s (n-1)
	|otherwise = s


{--Questão 21 ----------------------------------------------}
infix  8 &-
(&-)::Int->Int->Int
x &- y = x - 2*y

--Item 21.a
-- 0,  por conta da operação primeiro resolver "10 &- 3 = 4" e então "4 &- 2 = 0"
--Item 21.b
-- 12, por resolver primeiro "3 &- 2 = -1" e então "10 &- -1 = 12"
--Item 21.c
-- Erro de uso ambíguo de operador. Não foi definido qual dos "&-" deve ser resolvido primeiro, portanto o resultado pode ser 0 ou 12, uma ambiguidade.

--Item 21.a
-- (-2), pois a multiplicação tem prioridade 7, maior que a prioridade 6 do "&-", portanto os "*" ocorrem primeiro neste caso.
--Item 21.b
-- 8, afinal agora "&-" passa a ter uma prioridade maior que a multiplicação.


{--Questão 22 ----------------------------------------------}
inverte::[Int]->[Int]
inverte [] = []
inverte (a:b) = inverte b ++ [a]


{--Questão 23 ----------------------------------------------}
converte::[Int]->[Char]
converte [] = ""
converte (a:b) = (chr (64+a) : converte b)


{--Questão 24 ----------------------------------------------}
--Item 21.a  ['a','b','c','d','e','f','g']
--Item 21.b  [0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9]
--Item 21.c  [0.1,0.3,0.5,0.7,0.9]
--Item 21.d  [0.1,0.3,0.5,0.7,0.9,1.1,1.3,1.5,1.7,1.9]
--Item 21.e  []
--Item 21.f  [1,4,7,10,13]


{--Questão 25 ----------------------------------------------}
--O compilador não aceita o segundo termo como Char, apenas [Char], contradizendo o enunciado.
conta::[Char]->[Char]->Int
conta [] _ = 0
conta (a:b) [c]
	|a==c = (conta b [c]) + (1)
	|otherwise = conta b [c]


{--Questão 26 ----------------------------------------------}
purifica::[Int]->[Int]
purifica [] = []
purifica (a:[]) = (a:[])
purifica (a:(c:b))
	|a==c = purifica (c:b)
	|otherwise = (a : purifica (c:b))


{--Questão 27 ----------------------------------------------}
repete::Int->Int->[Int]
repete _ 0 = []
repete x y = (x : repete x (y-1))

proliferaInt::[Int]->[Int]
proliferaInt (a:[]) = repete a a
proliferaInt (a:b) = repete a a ++ proliferaInt b


{--Questão 28 ----------------------------------------------}
--"proliferaChar [C,B,D]", como está no enunciado, não funciona, apenas "proliferaChar ['C','B','D']"
repeteC::Char->Int->String
repeteC _ 0 = ""
repeteC y x = [y] ++ repeteC y (x-1)

proliferaChar::[Char]->String
proliferaChar (a:[]) = repeteC a (ord a - 64)
proliferaChar (a:b) = repeteC a (ord a - 64) ++ proliferaChar b
