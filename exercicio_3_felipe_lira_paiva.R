# Exercício 3 - Felipe Lira Paiva

install.packages("scales")
install.packages("ggbeeswarm")


library(tidyverse)
library(poliscidata)
library (scales)
library (ggbeeswarm)

# Utilizando o banco world do pacote poliscidata, faça um  
# histograma que também indique a média e um boxplot 
# da variável gini10
# Descreva o que você pode observar a partir deles.

      questao1 <- world

    # Histograma:
      
      ggplot (questao1, aes(gini10))+
        geom_histogram()+
        geom_vline(aes(xintercept = mean(gini10, na.rm = T)))
      
    # A média está por volta do valor 0,40 mas a moda está mais próxima do 0,35 (com 15 casos);
    # Não existem muitos casos após o 0,60 mas existe um em quase 0,75 (que será o outlier do boxplot).
      
    # Boxplot:
      ggplot (questao1, aes(gini10))+
        geom_boxplot()

    # Existe um caso desviante (outlier), representado pelo pontinho preto;
    # Há menos variação entre o valor mínimo e o primeiro quartil
    # em relação ao valor máximo e o terceiro quartil;
    # A mediana (ou segundo quartil) está levemente abaixo de 40

            
# Utilizando as funções de manipulação de dados da aula passada,
# faça uma tabela que sumarize a media (função mean), 
# mediana (funcao median) e o desvio padrão (fundao sd) da 
# renda per capta (variável gdppcap08), agrupada por tipo de regime 
# (variável democ).
# Explique a diferença entre valores das médias e medianas.
# Ilustre com a explicação com gráfico de boxplot.
# Os dados corroboram a hipótese da relação entre democracia
# e desempenho economico?


      questao2 <- world

      questao2 %>%
        group_by(democ) %>%
        summarise (media = mean(gdppcap08, na.rm = TRUE),
                   mediana = median(gdppcap08, na.rm = TRUE),
                   desvio_padrao = sd(gdppcap08, na.rm = TRUE))
    # A média significa somar todos os valores e dividir pelo número de casos.
    # No nosso caso: somar todos o pib per capita de uma democracia e divir pelo n de democracias;
    # e o mesmo para as não democracias.
    # A mediana, por sua vez, é o valor que divide o número de casos;
    # Ou seja, metade das democracias tem um pibpcap maior 11660 e a outra metade, menor.

      ggplot (questao1, aes(x = democ, y = gdppcap08))+
        geom_boxplot()
    
    # Sim. Ainda que existam outliers (especialmente para não democracia),
    # A média dos países democráticos é consideravelmente maior que as do não democráticos,
    # Inclusive, o boxplot permite algo interessante: a mediana da democracia é aproximadamente
    # o valor do terceiro quartil da não democracia. Enquanto o primeiro quartil democrático
    # está próximo da mediana não democrática.
      
# Carregue o banco states que está no pacote poliscidata 
# Mantenha apenas as variáveis obama2012, conpct_m, hs_or_more,
# prcapinc, blkpct10, south, religiosity3, state

    questao3 <- states %>%
      select (obama2012, conpct_m, hs_or_more, prcapinc, blkpct10, south, religiosity3, state)
    
    # Percent mass public Conservative
    
# Carregue o banco nes que está no pacote poliscidata
# Mantenha apenas as variáveis obama_vote, ftgr_cons, dem_educ3,
# income5, black, south, relig_imp, sample_state

    questao4 <- nes %>%
      select(obama_vote, ftgr_cons, dem_educ3, income5, black, south, relig_imp, sample_state)

# As variáveis medem os mesmos conceitos, voto no obama em 2012, 
# conservadorismo, educação, renda, cor, norte-sul, 
# religiosidade e estado. A diferença é que o nes é um banco de
# dados com surveys individuais e o states é um banco de dados
# com informações por estado
#
# Faça um gráfico para cada banco representando o nível de
# conservadorismo. Comente as conclusões que podemos ter
# a partir deles sobre o perfil do eleitorado estadunidense.
# Para ajudar, vocês podem ter mais informações sobre os bancos
# de dados digitando ?states e ?nes, para ter uma descrição das
# variáveis (conpct_m e ftgr_cons)

# Gráficos de densidade
    ggplot(questao3, aes(conpct_m))+
      geom_density()
    
    ggplot(questao4, aes(ftgr_cons))+
      geom_density(adjust = 2)
    
      
# Histogramas com curva de densidade e média   
    ggplot (questao3, aes(conpct_m))+
      geom_histogram(aes(y = ..density..),
                     binwidth = 2)+
      geom_density()+
      geom_vline(aes(xintercept = mean(conpct_m, na.rm = T)))
    
    
    ggplot (questao4, aes(ftgr_cons))+
      geom_histogram(aes(y = ..density..),
                     binwidth = 10)+
      geom_density()+
      geom_vline(aes(xintercept = mean(ftgr_cons, na.rm = T)))
    
    
  # Não entendi se era para fazer algum gráfico específico,
  # então fui testando e deixei os pq achei melhores; especialmente o histograma.

    
# Qual é o tipo de gráfico apropriado para descrever a variável
# de voto em obama nos dois bancos de dados?
# Justifique e elabore os gráficos
# obama2012 (%) e obama_vote (sim/nao)
  
  # No states, o melhor mesmo seria um mapa com o percentual dentro;
  # mas do que sei fazer, achei o geom_point no livro (achei um pouco + visual que barra)
  # mas ainda assim não é tão agradável aos olhos.
  
    ggplot(questao3, aes(obama2012, state))+
      geom_point()
    
  # Usei isso que encontrei no html. Fiz com estado e ficou igual a de cima.
  # Então coloquei sem estado mesmo. Dá pra ver os pontos e a porcentagem aprx de cada,
  # Mas não é tão útil por não ter o estado.
    ggplot(questao3, aes(obama2012,"")) +
      geom_beeswarm()
    
  # Vi no grupo que era para fazer só com uma variável, então fiz também
  # um gráfico de barras com o density, já que o normal fica muito "estranho"
  # por cada porcentagem diferir até nos decimais.
    
    ggplot(questao3, aes(obama2012))+
      geom_histogram(aes(y = ..density..),
                      binwidth = 10)+
      geom_vline(aes(xintercept = mean(obama2012, na.rm = T)))
        
    
  # No nes, um gráfico de barras:
    ggplot(questao4, aes(obama_vote))+
      geom_bar()
      

# Crie dois bancos de dados a partir do banco nes, um apenas com
# respondentes negros e outro com não-negros. A partir disso, faça
# dois gráficos com a proporção de votos no obama.
# O que você pode afirmar a partir dos gráficos?
# Você diria que existe uma relação entre voto em Obama e cor?

    ?nes
    bnegros <- nes %>%
      transmute(obama_vote, ftgr_cons, dem_educ3, income5, south, relig_imp, sample_state, black) %>%
      filter(black == "Yes")
            
    ggplot(bnegros, aes(obama_vote, ..count../sum(..count..)))+
      geom_bar()+
      scale_y_continuous(labels = percent)
    
    
    bbrancos <- nes %>%
      transmute(obama_vote, ftgr_cons, dem_educ3, income5, south, relig_imp, sample_state, black) %>%
      filter(black == "No")  

    ggplot(bbrancos, aes(obama_vote, ..count../sum(..count..)))+
      geom_bar()+
      scale_y_continuous(labels = percent)
    
    
  # Com os gráficos, podemos afirmar que existe uma relação entre
  # votar no Obama e a cor da pele. As pessoas negras votaram, majoritariamente, nele;
  # enquanto os votos das pessoas brancas foi muito mais dividido entre Obama e Romney, 
  # ainda que o Obama tenha tido um pouco mais de votos.
  # Ou seja, se negro, o voto no Obama é muito provável;
  # Se branco, a distribuição entre os dois cadidatos é mais próxima.
      
    
# A partir do banco de dados states, faça uma comparação semelhante.
# Faça um gráfico com as porcentagens de votos em Obama para estados
# que estão acima da mediana da porcentagem de população negra nos estados,
# e outro gráfico com as porcentagens de votos em Obama para os estados
# que estão abaixo da mediana de população negra.
# O que você pode afirmar a partir dos gráficos?
# Podemos chegar a mesma conclusão anterior?

      questao5 <- states %>%
        select (obama2012, conpct_m, hs_or_more, prcapinc, blkpct10, 
              south, religiosity3, state)
      
  # Passo 1: descobrir a mediana do percentual de negros (Resposta: 8.25)
      questao5 %>% 
       summarise (mediana = median(blkpct10, na.rm = TRUE))
  
  # Passo 2: gráfico c/ % de votos em Obama para estados > mediana da pop negra.
      
      questao5A <- questao5 %>%
        select (obama2012, conpct_m, hs_or_more, prcapinc, blkpct10, 
                south, religiosity3, state) %>%
        filter (blkpct10 > 8.25)
      
      #Pontos    
      ggplot (questao5A, aes (obama2012,state))+
        geom_point()
     
       #Histograma
      ggplot(questao5A, aes(obama2012))+
        geom_histogram(aes(y = ..density..),
                           binwidth = 10)+
        geom_vline(aes(xintercept = mean(obama2012, na.rm = T)))
   
  # Passo 3: gráfico c/ % de votos em Obama para estados < mediana da pop negra.
      
      questao5B <- questao5 %>%
        select (obama2012, conpct_m, hs_or_more, prcapinc, blkpct10, 
                south, religiosity3, state) %>%
        filter (blkpct10 < 8.25)
      
      #Pontos
      ggplot (questao5B, aes (obama2012,state))+
        geom_point()
      
      #Histograma
      ggplot(questao5B, aes(obama2012))+
        geom_histogram(aes(y = ..density..),
                       binwidth = 10)+
        geom_vline(aes(xintercept = mean(obama2012, na.rm = T)))
    

    # A partir do gráfico, podemos observar algumas coisas interessantes:
    # 1. Os estados abaixo da mediana variam mais (mais colunas; menor densidade);
    # 2. A média, em ambos, está próxima do valor 50;
    # 3. A maioria dos estados do primeiro gráfico tem uma grande concentração
    # nos valores 40, 50 e 60; enquanto que o segundo também tem 20, 30 (mas também 70).
    # Eu diria que é mais difícil chegar a mesma conclusão anterior,
    # Já que o gráfico anterior permite comparar melhor negros e não negros, além da
    # variação do par anterior ser muito mais acentuada.
      
      
# A partir da varíavel X do banco df abaixo
    df <- data.frame(x = cos(seq(-50,50,0.5)))
# Faça os tipos de gráficos que representem esse tipo de variável
# comentando as diferenças entre elas e qual seria a mais adequada

    # Densidade (eu acho que esse seria o mais indicado porque
    # foi o que eu achei quando procurei por cosseno)
      ggplot (df, aes(x))+
        geom_density()
  
    # Histograma
      ggplot (df, aes(x))+
        geom_histogram()
      
    # Barra (o pior)
      ggplot (df, aes(x))+
        geom_bar()
      
    # O de barra é o pior, por não permitir uma boa visualização dos dados.
    # Tanto o histograma quanto a curva de densidade são mais visíveis que o anterior,
    # mas acredito que a melhor escolha seria a de densidade.
      
      
# responsa as questões teóricas abaixo
      
# 1. Observar a figura 1.2 do livro Fundamentals of Political Research e
# fazer o mesmo esquema para o trabalho final de vocês.
#      
#   VI (+ Fragmentação Partidária) ------------------------> VD (+ cargos no executivo)
#
#   VI (+ Número Efetivo de Partidos Parlamentares) -------> VD (+ secretarias estaduais)
# 
#      
#
# 2. Qual é a disponibilidade de dados para sua pesquisa? Já existem
# bancos de dados prontos? Você tem acesso a eles? Caso a última
# pergunta seja positiva, responda o exerício 4 do capítulo 5.
#
#       Há dados disponível. Sim, existem. Sim, tenho eles.
# 2a) Describe the data set and the purpose for which it was assembled.
#     O objetivo do banco de dados era disponibilizar informações sobre os
#     secretários estaduais. Porém, eu não o utilizo para isso. Para mim, é
#     necessário apenas o número de secretarias; sem informação sobre quem as ocupa.
#
# 2b) What are the time and space dimensions of the data set?
#     Todos os estados do Brasil + DF. de 1995 a 2018.
#     Porém, alguns estados (4) têm apenas até 2014.
#
# Read the details of how one of the variables in which you are interested was
#   coded. Write your answers to the following questions:
# 
# !!! Estou respondendo a 3. aqui também !!!
# 2c) Does this seem like a reliable method of operationalizing this variable?
#  How might the reliability of this operationalization be improved?
#     Sim, parece-me confiável por se basear em dados oficiais (nomeações) que
#     são disponibilizados através dos diários oficiais dos estados.
#     
# 2d) Assess the various elements of the validity for this variable operationalization.
#   How might the validity of this operationalization be improved?
#     A validade também me parece sem problemas. Entretanto, talvez secretarias
#     não sejam a única forma de cargo que a teoria fala. Então isso significa que
#     o ideal seria acesso a outros dados (subsecretários, por exemplo). Entretanto,
#     não me proponho a fazer isso em razão de querer trabalhar com 24 estados em 20 anos,
#     então é um trabalho que está fora da minha capacidade de tempo/recursos.
#
#      
# 3.A partir dos exercícios anteriores, escreva sobre a confiabilidade e
#   validade de suas variáveis.
# 
# 4. Qual seria a forma ideal e mais adequada de operacionalizar suas
#  variáveis para testar sua hipótese?
#     Fragmentação partidária -> NEPP (Laakso e Taagepera, 1979) por ser
#     o mais consagrado e aceito na literatura, além de haver dados (TSE)
#     Número de cargos no executivo: número de secretarias estaduais.
