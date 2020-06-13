# 
# Exercício 1 de Análise de Dados 2020.1. Felipe Lira Paiva.
#
# Eleições presidenciais do Brasil em 2002.


# Crie um vetor com o nome dos seis candidatos à presidência.

candidato <- c("Luiz Inácio Lula da Silva", "José Serra", "Anthony Garotinho", "Ciro Gomes", "José Maria de Almeida", "Rui Costa Pimenta")


# Crie um vetor com a sigla do partido de cada candidato.

partido <- c("PT", "PSDB", "PSB", "PPS", "PSTU", "PCO")


# Crie um vetor com o total de votos de cada candidato.

votos_candidatos <- c(39455233, 19705445, 15180097, 10170882, 402236, 38619)


# Crie um objeto calculando a soma do votos dos candidatos no 1o turno

total_votos <- sum(votos_candidatos)


# Crie um vetor com a porcentagem de votos de cada candidato
# (devo dividir o número de cada um pelo número total)

porcentagem_votos <- (votos_candidatos / total_votos) * 100


# Crie uma matriz que conste uma coluna com o total de votos de cada candidato
# e outra com a porcentagem dos votos de cada candidato
# Ou seja: duas colunas e matriz numérica

matriz_votos <- matrix(c(votos_candidatos, porcentagem_votos), byrow = FALSE, nrow = 6)

# Para mim mesmo: usei o FALSE porque os dados (VETORES) estão dispostos por COLUNA.
# Eu tenho os vetores com as informações de candidatos, porcentagem, etc, para cada um dos candidatos,
# Por isso são dados em colunas (FALSE) e não em linhas (TRUE)


# Nomeie as linhas da matriz com o nome dos candidatos

rownames(matriz_votos) <- candidato


# Nomeie também as colunas
colnames(matriz_votos) <- c("Total de votos", "Porcentagem")



# Crie um dataframe com o nome dos candidatos, o partido,
# a quantidade de votos e o percentual

eleicao2002 <- data.frame(candidato, partido, votos_candidatos, porcentagem_votos)


# Crie um vetor lógico, indicado TRUE ou FALSE, com a informacao se
# o candidato foi para o segundo turno

segundoturno <- c(T, T, F, F, F, F)


# Adicione esta coluna no dataframe

eleicao2002 <- cbind(eleicao2002, segundoturno)


# Calcule a soma da porcentagem dos dois candidatos que obtiveram mais votos

eleicao2002[1,4] + eleicao2002[2,4]


# Exiba as informações do dataframe dos dois candidatos com mais votos
# Fiz de formas diferentes para ir testando

candidatosno2 <- eleicao2002$porcentagem_votos > 22

# Para exibir só os dois:
eleicao2002[candidatosno2, ]


candidatosno2nd <- eleicao2002$segundoturno == "TRUE"

# Para exibir só os dois:
eleicao2002[candidatosno2nd, ]


################################################################################

# Substitua o símbolo de interrogação por um 
# código que retorne o seguinte resultado:
#
# [1] 24 18 31

q <- c(47, 24, 18, 33, 31, 15)
q[c(2, 3, 5)]


###############################################################################

# Substitua o símbolo de interrogação por um 
# código que retorne o seguinte resultado:
#
# Out Nov
#  24   2

    x <- c(5, 4, 24, 2)
    y <- c("Ago", "Set", "Out", "Nov")
      names(x) <- y

        x[c(3,4)]



###############################################################################

# Substitua o símbolo de interrogação por um 
# código que retorne o seguinte resultado:
#
# 'data.frame':	2 obs. of  2 variables:
# $ x: Factor w/ 2 levels "d","e": 1 2
# $ y: num  1 4

df <- data.frame(x = c("d", "e"),
                   y = c(1,4)
                    )   

      str(df)

# SOS PROF. O senhor poderia comentar isso acima? Eu não achei o X ou Y no texto em html,
# eu consegui resolver na tentativa e erro, mas eu não entendi 100% porque é uma forma diferente de
# criar um dataframe em relação ao que fiz na linha 57, sem X e Y.


###############################################################################

# Crie a seguinte matriz
#
#       [,1] [,2] [,3]
# [1,]   19   22   25
# [2,]   20   23   26
# [3,]   21   24   27

vetor1 <- c(19,22,25)
vetor2 <- c(20,23,26)
vetor3 <- c(21,24,27)

        seguinte <- matrix(c(vetor1,vetor2,vetor3), byrow=TRUE, nrow = 3)

# Para mim mesmo: usei o TRUE pq o FALSE deu errado. Logo, o true está com meus vetores por linhas.
# Por isso, se os dados estiverem por coluna deve usar FALSE.


###############################################################################

# Se Z é uma matriz 4 por 4, qual é o resultado de Z[1,4] ?

# Se a matriz for distribuída pelas colunas (padrão), z[1,4] é "1"
z <- matrix(c(1:4), nrow = 4, ncol = 4)

# Para ver a resposta:
z[1,4]

# Se a matriz for distribuída pelas linhas, z[1,4] é "4"
z2 <- matrix(c(1:4), byrow = TRUE, nrow = 4, ncol = 4)

# Para ver a resposta:
z2[1,4]


###############################################################################

# Substitua o símbolo de interrogação por um 
# código que retorne o seguinte resultado:
#
#  W3 W4 W1 W2 
#  20 69  5 88 

      y <- c(20, 69, 5, 88)
        q <- c("W3", "W4", "W1", "W2")
          names(y) <- q


# Para ver a resposta:
y

###############################################################################

# Substitua o símbolo de interrogação por um 
# código que retorne o seguinte resultado:
#
#       [,1] [,2]
# [1,]    4    6
# [2,]    3    7
# [3,]    1    8


exercicio <- cbind(c(4, 3, 1), c(6, 7, 8))


###############################################################################

# Substitua o símbolo de interrogação por um 
# código que retorne o seguinte resultado:
#       [,1] [,2] [,3] [,4]
# [1,]    1    3   13   15
# [2,]    2    4   14   16

x <- 1:4
y <- 13:16

basenova <- matrix(c(x,y),
       nrow = 2,
       byrow = FALSE)
       

###############################################################################

# Crie o seguinte dataframe df
#
# df
#    x  y    z
# 1 17  A  Sep
# 2 37  B  Jul
# 3 12  C  Jun
# 4 48  D  Feb
# 5 19  E  Mar


df  <- data.frame(x = c(17,37,12,48,19), 
                    y = c("A", "B", "C", "D", "E"), 
                      z= c("Sep","Jul","Jun","Feb","Mar")
                        )



# Ainda utilizando o dataframe df,
# qual código produziria o seguinte resultado?
#
#    x  y
# 1 17  A
# 2 37  B
# 3 12  C


resultado <- df[1:3,1:2]

# Para mim mesmo: lembre que [(LINHA),(COLUNA)]. O primeiro está selecionando as linhas 1-4.
# O segundo está selecionando as colunas 1 e 2.
# As fórmulas de matrizes funcionam aqui, pelo que enntendi. Confirmar se é sempre.




###############################################################################

# Exercício teórico


# Professor, eu espero que não tenha problema, mas estou utilizando o que fiz no meu TCC,
# pois eu fiz meu TCC majoritariamente no SPSS e, antes, usei o excel para limpar os dados,
# daí, para transformá-lo em artigo, eu estou revisando os dados e gostaria de fazer tudo no R.
# Por isso, pensei em trabalhar os dados desse artigo (ex TCC) na disciplina como meu trabalho final.
# Se não puder fazer isso, o sr me avisa e procurarei um tema.
# Algumas coisas que eu vou fazer, também servirão para o projeto do Mestrado,
# já que uso o mesmo banco de dados para a minha VD.


# Tema: Fragmentação partidária e número de secretarias estaduais (1995-2018)
# Hipótese principal: Quanto maior a fragmentação partidária, maior o número de secretarias estaduais.


# Explicação causal:
# [1] Amorim Neto (2000) e Raile, Pereira e Power (2009): ministérios podem servir como 
# arranjo das coalizões, pois o Executivo possui cargos a oferecer em troca de apoio no Legislativo;
# [2] Borges e Barbosa (2019): observa-se que o aumento no número de partidos na coalizão e
# no número de ministérios é, em geral, concomitante;
# [3] O Brasil é federalista (SOUZA, 2005), porém há diferenças entre os níveis 
# de governo (MELO, 2010) e, ainda, diferenças entre os estados (TOMIO e RICCI, 2012);
# [4] Os estados também apresentam alta fragmentação partidária, ainda que em valores diferentes 
# COSTA e BOLOGNESI, 2014; ROCHA, 2015; OLIVEIRA, 2016; BORGES, 2018; GRAÇA e PINTO, 2018; LACERDA, 2018).

# Partindo, então, da hipótese central de Borges e Barbosa (2019) e 
# do mecanismo de Amorim Neto (2000) e Raile, Pereira e Power (2009), 
# este trabalho aplica tais noções ao nível estadual, 
# pois há instituições semelhantes (SOUZA, 2005) e 
# é necessário entender cada nível da federação por si só e não um pelo outro (MELO, 2010). 



# Operacionalização das variáveis

# [1] Fragmentação partidária. Base de dados do TSE.
# Aqui, eu vou precisar transformar os dados das eleições no NEPP.
# Quando eu fiz para o TCC, eu fiz com uma fórmula no excel e conferi com a calculadora eleitoral.
# Eu acho que se eu fizer uma fórmula, posso jogar todos os dados e isso ser relativamente "simples".


# [2] Número de secretarias estaduais. Banco de dados do CEPESP DATA
# http://cepespdata.io/consulta/secretarios
# Aqui, é mais complicado, pois o banco está organizado por secretário, não secretaria.
# Cada secretário é adicionado de acordo com o ano que assumiu e deixou o cargo.
# Logo, há secretários que ficaram 2, 3 ou 4 anos (um mandato de governador) e são contados apenas uma vez.
# Eu precisarei, de alguma forma, desmembrar essa informação em anos 
# (para saber quantas secretarias existiram num ano)

# Porém, há outro problema: num mesmo ano, dois secretários (ou mais) podem ter assumido a mesma pasta.
# Exemplo: 1o secretário meses de Jan-Mar; 2o Mar-Nov; 3o Nov-Dez.
# Então, para resolver isso, talvez seja melhor desmembrar em meses, não em anos.

# E, ainda, há outro problema que é solucionado com o desmembramento em meses:
# Entre Jan-Jun pode ter existido 20 secretarias diferentes e com uma com o tema "Tecnologia"
# Entre Jun-Dez pode ter existido outras 20 secretarias, mas sem essa de "Tecnologia"
# e com uma de "Reforma Agrária", que não havia antes. 
# Se eu contar as secretarias que existiram ao longo do ano, são 21 diferentes, mas, ao mesmo tempo, apenas 20.


# Eu ainda não sei se é possível fazer tudo no R (acredito que sim).


