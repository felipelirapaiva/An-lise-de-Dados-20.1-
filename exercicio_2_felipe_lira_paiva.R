#
# Exercício 2 - Felipe Lira Paiva
#

install.packages("tidyverse")
library(tidyverse)

install.packages("poliscidata")
library(poliscidata)

#
# Suponha que tenhamos o dataframe df abaixo
#
# x     y
# A     5
# A     3
# B     8
# B    12
#
# Complete o código que obtém o seguinte resultado:
#
#        z
#        7
#

      questao1 <- data.frame (x = c("A","A","B","B"), 
                            y = c(5,3,8,12)) %>% 
                  summarise(z = mean(y))

# Para ver a resposta
      questao1

    
#######################################################################

# Suponha que tenhamos o dataframe df abaixo
#
# y1    y2    y3    y4
# 8.04  9.14  7.46  6.58
# 6.95  8.14  6.77  5.76
# 7.58  8.74  12.74 7.71
#
# Complete o código que obtém o seguinte resultado:
#
# y1    
# 8.04  
# 6.95  
# 7.58  

      questao2 <- data.frame (y1 = c(8.04, 6.95, 7.58),
                            y2 = c(9.14, 8.14, 8.74),
                            y3 = c(7.46, 6.77, 12.74),
                            y4 = c(6.58, 5.76, 7.71)) %>%
                  select("y1")
  
# Para ver a resposta
      questao2
    
                  
#######################################################################

# Suponha que tenhamos o dataframe df abaixo
#
#    x  y
#   1  10
#   6  8
#   2  3
#   4  5
#
# Complete o código que obtém o seguinte resultado, fazendo uma operação
# entre x e y
#
#    x  y   z
#   1  10  -9
#   6  8   -2
#   2  3   -1
#   4  5   -1
#


      questao3 <- data.frame (x = c(1, 6, 2, 4),
                            y = c(10, 8, 3, 5)) %>%
                  mutate(z = x - y)
    
# Para ver a resposta  
      questao3
        
########################################################################

#
# Suponha que tenhamos o dataframe df abaixo
#
#    city sales
# Boston   220
# Boston   125
#    NYC   150
#    NYC   250
#
# Complete o código que obtém o seguinte resultado:
#
# city   avg_sales
# Boston      172
# NYC         200 

      questao4 <- data.frame(city  = c("Boston", "Boston", "NYC", "NYC"),
                             sales = c(220, 125, 150, 250)) %>%
                  group_by(city) %>%
                  summarise(avg_sales = mean(sales))
      
# Para ver a resposta
      questao4

            
########################################################################

# Suponha que tenhamos o dataframe df abaixo
#
#week   min   max
#  3    55    60
#  2    52    56
#  1    60    63
#  4    65    67
#
# Complete o código que obtém o seguinte resultado:
#
#week   min   max
#  1    60    63
#  2    52    56
#  3    55    60
#  4    65    67

      questao5 <- data.frame(week = c(3, 2, 1, 4),
                             min = c(55, 52, 60, 65),
                             max = c(60,56,63,67)) %>%
                  arrange(week)
      
# Para ver a resposta
      questao5
        
        
########################################################################

# Suponha que tenhamos o dataframe df abaixo
#
# x_b_1  x_b_2  y_c_1  y_c_2
#  A      2      W1     25
#  A      4      W2     21
#  B      6      W1     26
#  B      8      W2     30
#
# Complete o código que obtém o seguinte resultado:
#
# y_c_1  y_c_2
#  W1     25
#  W2     21
#  W1     26
#  W2     30

      questao6 <- data.frame (x_b_1 = c("A", "A", "B", "B"),
                              x_b_2 = c(2, 4, 6, 8),
                              y_c_1 = c("w1", "w2", "w1", "w2"),
                              y_c_2 = c(25, 21, 26, 30)) %>%
                 
         select (starts_with("y"))
      
# Para ver a resposta
      questao6
      
      
#########################################################################

# Suponha que tenhamos o dataframe df abaixo
#
# Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
# 78           6.7         3.0          5.0         1.7 versicolor
# 121          6.9         3.2          5.7         2.3  virginica
# 11           5.4         3.7          1.5         0.2     setosa
# 92           6.1         3.0          4.6         1.4 versicolor
# 146          6.7         3.0          5.2         2.3  virginica
# 62           5.9         3.0          4.2         1.5 versicolor
# 50           5.0         3.3          1.4         0.2     setosa
# 17           5.4         3.9          1.3         0.4     setosa
# 69           6.2         2.2          4.5         1.5 versicolor
# 143          5.8         2.7          5.1         1.9  virginica
#
# Complete o código que obtém o seguinte resultado:
#
#Species      Sepal.Area
#versicolor      20.10
#virginica       22.08
#setosa          19.98
#versicolor      18.30
#virginica       20.10
#versicolor      17.70
#setosa          16.50
#setosa          21.06
#versicolor      13.64
#virginica      15.66

      questao7 <- iris[c(78,121,11,92,146,62,50,17,69,143),] %>%
                  transmute (Species,
                             Sepal.Area = Sepal.Length * Sepal.Width,)                
      
# Para ver a resposta:
      questao7

########################################################################

# Suponha que tenhamos o dataframe df abaixo
#
#name         start       end         party     
#Eisenhower   1953-01-20  1961-01-20  Republican
#Kennedy      1961-01-20  1963-11-22  Democratic
#Johnson      1963-11-22  1969-01-20  Democratic
#Nixon        1969-01-20  1974-08-09  Republican
#Ford         1974-08-09  1977-01-20  Republican
#Carter       1977-01-20  1981-01-20  Democratic
#Reagan       1981-01-20  1989-01-20  Republican
#Bush         1989-01-20  1993-01-20  Republican
#Clinton      1993-01-20  2001-01-20  Democratic
#Bush         2001-01-20  2009-01-20  Republican
#Obama        2009-01-20  2017-01-20  Democratic
#
#Crie um código abaixo para que se altere a variável party
#deixando apenas a primeira letra dos partidos
      
      
      questao8 <- presidential %>%
                  mutate (party = recode (party,
                                          Republican = "R",
                                          Democratic = "D"))

# Para ver a resposta:      
      questao8

      
###############################################################################

# No pacote poliscidata existe um banco de dados chamado nes, com informações 
# do American National Election Survey. Para os exerícicios a seguir, instale 
# o pacote poliscidata e tidyverse, carregue-os e crie um objeto chamado
# df com os dados do nes. 

library(tidyverse)

      
install.packages("poliscidata")
library(poliscidata)
      
      df <- nes

# Faça uma primeira exploração do banco de dados com todos os comandos
# passados até aqui que possuem esse objetivo

      
      glimpse(df) # N de linhas, N de colunas, nomes, tipos, primeiros casos...
      head(df) # Primeiras linhas
      tail(df) # Últimas linhas
      str (df) # N de linhas, N de colunas, nomes, tipos, primeiros casos...
      summary (df) # Descrição das colunas, por tipo (vi min, 1o quartil, yes/no...)
      
# Quantos respondentes possui na pesquisa?
# 5916
      
      
# Caso queiram ter mais informações sobre as variáveis do nes, basta rodar
# o código `?nes`, que no canto inferior direito aparecerá uma descrição.
# Como temos muitas variáveis, deixe apenas as colunas
# ftgr_cons, dem_raceeth, voted2012, science_use, preknow3, obama_vote
# income5, gender.

      ?nes

      df_selecionado <- df %>%
                        select (ftgr_cons, dem_raceeth, voted2012, science_use, preknow3, obama_vote, income5, gender)
      
# Se quisermos ter informações apenas de pessoas que votaram na
# eleição de 2012, podemos usar a variável voted2012. Tire do banco
# os respondentes que não votaram
      
      df_selecionado <- df %>%
        select (ftgr_cons, dem_raceeth, voted2012, science_use, preknow3, obama_vote, income5, gender) %>%
        filter (voted2012 == "Voted")
      

# Quantos respondentes sobraram?
      glimpse(df_selecionado)

#  Sobraram 4404
      

# Crie uma variável chamada white que indica se o respondente é branco
# ou não a partir da variável dem_raceeth, crie a variável ideology a
# partir da variável ftgr_cons (0-33 como liberal, 34 a 66 como centro,
# e 67 a 100 como conservador), ao mesmo tempo em que muda
# a variável obama_vote para trocar o 1 por "Sim" e 0 por "não"
      
    glimpse(df_selecionado) 
    # Anotações: ftgr_cons é "dbl" / dem_raceeth é factor / obama_vote é "dbl"
      
      df_selecionado <- df %>%
        select (ftgr_cons, dem_raceeth, voted2012, science_use, preknow3, obama_vote, income5, gender) %>%
        filter (voted2012 == "Voted") %>%
        mutate (white = dem_raceeth == "1. White non-Hispanic") %>%
        mutate (ideology = case_when (ftgr_cons <= 33 ~ "Liberal",
                                      ftgr_cons > 33 & ftgr_cons < 67 ~ "Centro",
                                      ftgr_cons >= 67 ~ "Conservador"),
                obama_vote = case_when (obama_vote >= 1 ~ "Sim",
                                        obama_vote < 1 ~ "Não"))
        
        
# Demonstre como observar a quantidade de pessoas em cada uma das
# categorias de science_use


      df_selecionado %>% 
        count(science_use)
         

# Demonstre como observar a média de conservadorismo (variável 
# ftgr_cons) para cada categoria de science_use

      df_selecionado %>%
        group_by(science_use) %>%
        summarise(media = mean (ftgr_cons, na.rm = TRUE))
        
  
###############################################################################

# Responder as questões teóricas da aula abaixo

# Artigo: Ministérios como "barganha": coalizão de governo e organização do Poder Executivo (2019)
#         Autoras: Jaqueline da Silva Borges e Sheila Cristina Tolentino Barbosa.
      
      
      
#     1 - Qual é a questão da pesquisa?
#   A variação do número de ministérios e do tamanho da coalizão dos governos (Nº de partidos na coalizão)

#     2 - Qual é a teoria?
#   O artigo lida com o debate teórico do presidencialismo de coalizão; especialmente com os textos de
#   Amorim Neto (2006) e Raile, Pereira e Power (2011). Neles, o Executivo é visto como possuidor dos cargos,
#   enquanto o legislativo tem os votos. A relação é que os ministérios, então, seriam um instrumento de barganha.

#     3 - Qual é o desenho de pesquisa?
#   Metodologia qualitativa, explorando a frequência de gabinetes e dos partidos na coalizão.
#   Usam algumas técnicas quantitativas também, como a correlação (mas só essa mesmo).
#   Período: 1945-2016.

#     4 - Como o artigo se sai nos 4 quesitos de avaliação de causalidade? 
#   Importante destacar, primeiro, que as autoras afirmam explicitamente não buscarem uma relação de causalidade.
#   Na minha visão, isso é, sobretudo, em razão da avaliação 4 (possível outra variável Z):
#   Como elas estudam o Brasil, há apenas um caso variando no tempo. Então, elas não podem saber qual o efeito do tempo,
#   Ou qual o efeito dos partidos na presidência, do aumento da população, etc, já que tudo também varia junto para o único caso.
#   Considero-o bom nos quesitos 1 (mecanismo) e 3 (covariância). Eu acredito que o 2 não faria sentido,
#   já que seria estranho o número de ministérios causar mais partidos no gabinete, pois, primeiro, os partidos são eleitos,
#   (então já tem o número de partidos) e, só em seguida, é montada a coalizão para, enfim, ter o gabinete.
      
#     5 - O que ele conclui? 
#   Existe uma correlação entre o número de ministérios e o tamanho das coalizões.

#     6 - Como a sua pesquisa dá um passo a mais para o desenvolvimento teórico presente neste artigo?
#   A minha principal contribuição é oferecer uma variação espacial (estados do Brasil)
#   para além da variação temporal presente no artigo de Borges e Barbosa.  
    
      
#     Elabore qual é a pergunta da sua pesquisa em apenas uma frase  
#   Qual o efeito da fragmentação partidária no número de secretarias estaduais?

#     Pense no seu trabalho e avalie em que medida ele passa pelas 4
#     avaliações de relação causal, e quais problemas ele pode ter em cada uma delas.
#   (1) Argumentação razoável. Passa bem, dialoga com a literatura existente. Mas talvez não explique tudo.      
#   (2) Causalidade reversa. Passa bem, por causa da ordem dos eventos.
#   (3) Covariação. Passa razoavelmente bem. Quando fiz o TCC, alguns estados tinham uma covariação menor que outros.
#   (4) Confounding. Minha maior limitação. Não foi algo que refleti muito na elaboração do TCC e precisaria pensar mais.
#                   Os controles que havia adicionado Controles: Magnitude do distrito, arrecadação do ICMS, 
#                   PIB per capita e tamanho do gabinete federal
      
      
      
      


