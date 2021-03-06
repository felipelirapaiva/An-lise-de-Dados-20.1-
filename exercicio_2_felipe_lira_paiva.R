#
# Exerc�cio 2 - Felipe Lira Paiva
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
# Complete o c�digo que obt�m o seguinte resultado:
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
# Complete o c�digo que obt�m o seguinte resultado:
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
# Complete o c�digo que obt�m o seguinte resultado, fazendo uma opera��o
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
# Complete o c�digo que obt�m o seguinte resultado:
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
# Complete o c�digo que obt�m o seguinte resultado:
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
# Complete o c�digo que obt�m o seguinte resultado:
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
# Complete o c�digo que obt�m o seguinte resultado:
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
#Crie um c�digo abaixo para que se altere a vari�vel party
#deixando apenas a primeira letra dos partidos
      
      
      questao8 <- presidential %>%
                  mutate (party = recode (party,
                                          Republican = "R",
                                          Democratic = "D"))

# Para ver a resposta:      
      questao8

      
###############################################################################

# No pacote poliscidata existe um banco de dados chamado nes, com informa��es 
# do American National Election Survey. Para os exer�cicios a seguir, instale 
# o pacote poliscidata e tidyverse, carregue-os e crie um objeto chamado
# df com os dados do nes. 

library(tidyverse)

      
install.packages("poliscidata")
library(poliscidata)
      
      df <- nes

# Fa�a uma primeira explora��o do banco de dados com todos os comandos
# passados at� aqui que possuem esse objetivo

      
      glimpse(df) # N de linhas, N de colunas, nomes, tipos, primeiros casos...
      head(df) # Primeiras linhas
      tail(df) # �ltimas linhas
      str (df) # N de linhas, N de colunas, nomes, tipos, primeiros casos...
      summary (df) # Descri��o das colunas, por tipo (vi min, 1o quartil, yes/no...)
      
# Quantos respondentes possui na pesquisa?
# 5916
      
      
# Caso queiram ter mais informa��es sobre as vari�veis do nes, basta rodar
# o c�digo `?nes`, que no canto inferior direito aparecer� uma descri��o.
# Como temos muitas vari�veis, deixe apenas as colunas
# ftgr_cons, dem_raceeth, voted2012, science_use, preknow3, obama_vote
# income5, gender.

      ?nes

      df_selecionado <- df %>%
                        select (ftgr_cons, dem_raceeth, voted2012, science_use, preknow3, obama_vote, income5, gender)
      
# Se quisermos ter informa��es apenas de pessoas que votaram na
# elei��o de 2012, podemos usar a vari�vel voted2012. Tire do banco
# os respondentes que n�o votaram
      
      df_selecionado <- df %>%
        select (ftgr_cons, dem_raceeth, voted2012, science_use, preknow3, obama_vote, income5, gender) %>%
        filter (voted2012 == "Voted")
      

# Quantos respondentes sobraram?
      glimpse(df_selecionado)

#  Sobraram 4404
      

# Crie uma vari�vel chamada white que indica se o respondente � branco
# ou n�o a partir da vari�vel dem_raceeth, crie a vari�vel ideology a
# partir da vari�vel ftgr_cons (0-33 como liberal, 34 a 66 como centro,
# e 67 a 100 como conservador), ao mesmo tempo em que muda
# a vari�vel obama_vote para trocar o 1 por "Sim" e 0 por "n�o"
      
    glimpse(df_selecionado) 
    # Anota��es: ftgr_cons � "dbl" / dem_raceeth � factor / obama_vote � "dbl"
      
      df_selecionado <- df %>%
        select (ftgr_cons, dem_raceeth, voted2012, science_use, preknow3, obama_vote, income5, gender) %>%
        filter (voted2012 == "Voted") %>%
        mutate (white = dem_raceeth == "1. White non-Hispanic") %>%
        mutate (ideology = case_when (ftgr_cons <= 33 ~ "Liberal",
                                      ftgr_cons > 33 & ftgr_cons < 67 ~ "Centro",
                                      ftgr_cons >= 67 ~ "Conservador"),
                obama_vote = case_when (obama_vote >= 1 ~ "Sim",
                                        obama_vote < 1 ~ "N�o"))
        
        
# Demonstre como observar a quantidade de pessoas em cada uma das
# categorias de science_use


      df_selecionado %>% 
        count(science_use)
         

# Demonstre como observar a m�dia de conservadorismo (vari�vel 
# ftgr_cons) para cada categoria de science_use

      df_selecionado %>%
        group_by(science_use) %>%
        summarise(media = mean (ftgr_cons, na.rm = TRUE))
        
  
###############################################################################

# Responder as quest�es te�ricas da aula abaixo

# Artigo: Minist�rios como "barganha": coaliz�o de governo e organiza��o do Poder Executivo (2019)
#         Autoras: Jaqueline da Silva Borges e Sheila Cristina Tolentino Barbosa.
      
      
      
#     1 - Qual � a quest�o da pesquisa?
#   A varia��o do n�mero de minist�rios e do tamanho da coaliz�o dos governos (N� de partidos na coaliz�o)

#     2 - Qual � a teoria?
#   O artigo lida com o debate te�rico do presidencialismo de coaliz�o; especialmente com os textos de
#   Amorim Neto (2006) e Raile, Pereira e Power (2011). Neles, o Executivo � visto como possuidor dos cargos,
#   enquanto o legislativo tem os votos. A rela��o � que os minist�rios, ent�o, seriam um instrumento de barganha.

#     3 - Qual � o desenho de pesquisa?
#   Metodologia qualitativa, explorando a frequ�ncia de gabinetes e dos partidos na coaliz�o.
#   Usam algumas t�cnicas quantitativas tamb�m, como a correla��o (mas s� essa mesmo).
#   Per�odo: 1945-2016.

#     4 - Como o artigo se sai nos 4 quesitos de avalia��o de causalidade? 
#   Importante destacar, primeiro, que as autoras afirmam explicitamente n�o buscarem uma rela��o de causalidade.
#   Na minha vis�o, isso �, sobretudo, em raz�o da avalia��o 4 (poss�vel outra vari�vel Z):
#   Como elas estudam o Brasil, h� apenas um caso variando no tempo. Ent�o, elas n�o podem saber qual o efeito do tempo,
#   Ou qual o efeito dos partidos na presid�ncia, do aumento da popula��o, etc, j� que tudo tamb�m varia junto para o �nico caso.
#   Considero-o bom nos quesitos 1 (mecanismo) e 3 (covari�ncia). Eu acredito que o 2 n�o faria sentido,
#   j� que seria estranho o n�mero de minist�rios causar mais partidos no gabinete, pois, primeiro, os partidos s�o eleitos,
#   (ent�o j� tem o n�mero de partidos) e, s� em seguida, � montada a coaliz�o para, enfim, ter o gabinete.
      
#     5 - O que ele conclui? 
#   Existe uma correla��o entre o n�mero de minist�rios e o tamanho das coaliz�es.

#     6 - Como a sua pesquisa d� um passo a mais para o desenvolvimento te�rico presente neste artigo?
#   A minha principal contribui��o � oferecer uma varia��o espacial (estados do Brasil)
#   para al�m da varia��o temporal presente no artigo de Borges e Barbosa.  
    
      
#     Elabore qual � a pergunta da sua pesquisa em apenas uma frase  
#   Qual o efeito da fragmenta��o partid�ria no n�mero de secretarias estaduais?

#     Pense no seu trabalho e avalie em que medida ele passa pelas 4
#     avalia��es de rela��o causal, e quais problemas ele pode ter em cada uma delas.
#   (1) Argumenta��o razo�vel. Passa bem, dialoga com a literatura existente. Mas talvez n�o explique tudo.      
#   (2) Causalidade reversa. Passa bem, por causa da ordem dos eventos.
#   (3) Covaria��o. Passa razoavelmente bem. Quando fiz o TCC, alguns estados tinham uma covaria��o menor que outros.
#   (4) Confounding. Minha maior limita��o. N�o foi algo que refleti muito na elabora��o do TCC e precisaria pensar mais.
#                   Os controles que havia adicionado Controles: Magnitude do distrito, arrecada��o do ICMS, 
#                   PIB per capita e tamanho do gabinete federal
      
      
      
      


