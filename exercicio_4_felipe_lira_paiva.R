# Exerc�cio 4 - Felipe Lira Paiva


## Fa�a todos os gr�ficos utilizando um tema que voc� ache mais adequado
## e nomeie os eixos x e y da maneira adequada

## Carregue o banco world do pacote poliscidata

install.packages(vcd)

library(tidyverse)
library(poliscidata)
library (graphics)
library(vcd)
library(scales)

      banco <- world

      ?world

      
## Observe o banco de dados com as fun��es adequadas

      glimpse(banco) # N� de linhas e colunas; 1�s casos; tipos; nomes. 
      str(banco) # N� de linhas e colunas; 1�s casos; tipos
      summary(banco) # vari�veis (min, max, m�dia, 1st e 3rd quadrante...)
      head(banco) # primeiras linhas
      tail(banco) # �ltimas linhas

## A vari�vel democ_regime08 indica se um pa�s � democr�tico.
## Usando as ferramentas de manipulacao de bancos de dados, verifique
## quantos paises sao democraticos ou nao, e apresente esta vari�vel 
## graficamente

      banco %>%
        count(democ_regime08)

## 95 democr�ticos; 69 n�o democr�ticos; 3 sem dados

      ggplot(banco, aes(fct_infreq(democ_regime08)))+
        geom_bar()+
        theme_classic()+
        labs(title = "Figura 1",
           subtitle = "Gr�fico de pa�ses democr�ticos e n�o democr�ticos",
           x = "O pa�s � democr�tico?",
           y = "N�mero",
           caption = "Elabora��o pr�pria a partir do banco 'World' (R)")
      
      
## Teste a rela��o entre a vari�vel democ_regime08 e a vari�vel
## muslim (que indica se um pa�s � mu�ulmano ou n�o). E represente
## visualmente as vari�veis para visualizar se esta religi�o
## aumenta ou diminui a chance de um pa�s ser democr�tico
## Qual seria sua conclus�o com rela��o a associa��o destas duas
## vari�veis?

      
# Como s�o duas categ�ricas, tem que fazer um chi-quadrado
# Primeiro, a tabela com o n�mero total (table(x,y))
      tabela1 <- table(banco$muslim, banco$democ_regime08)
 
# Segundo, a tabela com a propor��o em rela��o � coluna
      prop.table(tabela1,2)

# Terceiro, o Chi-quadrado
      chisq.test(tabela1)

# Mosaic plot (topo = muslim / lado = democr�tico)
      mosaicplot(tabela1, shade = TRUE)
      
# Associa��o (topo = democr�tico / lado = muslim)
      assoc(tabela1, shade = TRUE)
      
# Gr�fico de barra
      ggplot(banco, aes(muslim, fill = democ_regime08)) +
        geom_bar(position = "fill")+
        theme_classic()+
        labs(title = "Figura 2",
             subtitle = "Gr�fico de pa�ses democr�ticos e n�o democr�ticos",
             x = "O pa�s � mu�ulmano",
             y = "N�mero",
             caption = "Elabora��o pr�pria a partir do banco 'World' (R)") 
      
# A partir dos dados acima, podemos concluir que existe uma rela��o
# positiva entre ser mu�ulmano e n�o democr�tico, a n�vel de pa�s;
# Uma associa��o negativa entre ser mu�ulmano e democr�tico, outra
# negativa entre n�o ser mu�ulmano e n�o ser democr�tico.
      
      
## A vari�vel gdppcap08 possui informa��o sobre o PIB per capta
## dos pa�ses. Fa�a uma representa��o gr�fica desta vari�vel

      ggplot(banco, aes(gdppcap08, ..count../sum(..count..)))+
        geom_density()+
        scale_y_continuous(labels = percent)+
        theme_classic()+
        labs(title = "Figura 3",
             subtitle = "Gr�fico de distribui��o de PIB per capita",
             x = "PIB per capita",
             y = "Porcentagem de pa�ses",
             caption = "Elabora��o pr�pria a partir do banco 'World' (R)") 
      


## Fa�a um sumario com a m�dia, mediana e desvio padr�o do pib per capta
## para cada tipo de regime politico, represente a associa��o destas
## vari�veis graficamente, e fa�a o teste estat�stico adequado para
## chegar a uma conclus�o. Existe associa��o entre as vari�veis?

# (1.1) O sum�rio sem NA:
      banco %>%
        filter(!is.na(democ_regime08),
               !is.na(gdppcap08)) %>%
             group_by(democ_regime08) %>%
        summarise(media = mean(gdppcap08, na.rm = TRUE),
                  mediana = median(gdppcap08, na.rm = TRUE),
                  desvio = sd(gdppcap08, na.rm = TRUE))
      
# (1.2) O sum�rio com NA:
      banco %>%
        group_by(democ_regime08) %>%
        summarise(media = mean(gdppcap08, na.rm = TRUE),
                  mediana = median(gdppcap08, na.rm = TRUE),
                  desvio = sd(gdppcap08, na.rm = TRUE))
    
# (2.1) Associa��o graficamente: com boxplot
      ggplot(banco, aes (democ_regime08, gdppcap08))+
        geom_boxplot()+
        theme_classic()+
        labs(title = "Figura 4",
             subtitle = "Boxplot de PIB per capita por pa�s",
             x = "O pa�s � democr�tico?",
             y = "PIB per capita",
             caption = "Elabora��o pr�pria a partir do banco 'World' (R)") 

# (2.2) Associa��o graficamente: com densidade      
      ggplot(banco, aes (gdppcap08, fill = democ_regime08))+
        geom_density(alpha = 0.5)+
        theme_classic()+
        scale_y_continuous(labels = percent)+
        labs(title = "Figura 5",
             subtitle = "Regime democr�tico e PIB per capita",
             x = "PIB per capita",
             y = "Porcentagem de pa�ses",
             caption = "Elabora��o pr�pria a partir do banco 'World' (R)") 
      
# (3) Teste-T
      t.test(gdppcap08 ~ democ_regime08, data = banco)
      
# A partir dos dados acima (Teste-T, gr�ficos), podemos dizer que sim,
# h� uma rela��o (ainda que n�o queira dizer causalidade) entre PIB per
# capita e democracia: tanto a mediana quanto os valores da democracia
# s�o levemente maiores. No Teste-T, a diferen�a das m�dias n�o � 
# igual a 0 e o p-valor � baixo (0,004). Acredito que podemos concluir
# que � estatisticamente significante.
# Aten��o para a exist�ncia de outliers que podem, possivelmente,
# representar monarquias ricas em recursos naturais e "pequenas".
# Num trabalho, talvez fosse interessante retir�-los e testar sem eles.
      
      
      
## Por fim, ao inv�s de utilizar uma vari�vel bin�ria de democracia,
## utilize a vari�vel dem_score14 para avaliar se existe uma associa��o
## entre regime pol�tico e desenvolvimento econ�mico. Represente
## a associa��o graficamente, fa�a o teste estat�stico e explica sua
## conclus�o

# (1) Gr�fico
      ggplot(banco, aes(dem_score14, gdppcap08))+
        geom_point()+
        theme_classic()+
        labs(title = "Figura 6",
             subtitle = "Gr�fico de valor democr�tico e PIB per capita",
             x = "Valor da democracia",
             y = "PIB per capita",
             caption = "Elabora��o pr�pria a partir do banco 'World' (R)") 
      
# (2) Correla��o
      cor.test(banco$dem_score14, banco$gdppcap08)
      

# Existe uma correla��o moderada (50.5), o que n�o � desprez�vel,
# O p-valor � baixo, ent�o o teste � estatisticamente significante.
# Existem outliers (pouco valor de democracia com PIBs muito altos),
# como vimos na quest�o anterior tamb�m.
      
      
## Teste a associa��o entre renda perca capta e religiao (com a vari�vel
## muslim) e represente graficamente. Qual � sua conclus�o? 

# (1) Associa��o com o teste-t
  
  t.test(gdppcap08 ~ muslim, data = banco)
    
# (2.1) Associa��o graficamente: com boxplot
  ggplot(banco, aes (muslim, gdppcap08))+
    geom_boxplot()+
    theme_classic()+
    labs(title = "Figura 7",
         subtitle = "Boxplot de PIB per capita por pa�s, se mul�umano",
         x = "O pa�s � mu�ulmano?",
         y = "PIB per capita",
         caption = "Elabora��o pr�pria a partir do banco 'World' (R)") 
  
# (2.2) Associa��o graficamente: com densidade      
  ggplot(banco, aes (gdppcap08, fill = muslim))+
    geom_density(alpha = 0.5)+
    theme_classic()+
    scale_y_continuous(labels = percent)+
    labs(title = "Figura 8",
         subtitle = "Religi�o mu�ulmana e PIB per capita",
         x = "PIB per capita",
         y = "Porcentagem de pa�ses",
         caption = "Elabora��o pr�pria a partir do banco 'World' (R)") 
  
# Sim, existe uma rela��o. Os pa�ses n�o mu�ulmanos possuem,
# em geral, uma renda maior que os pa�ses mu�ulmanos.
# � poss�vel ver isso a partir da diferen�a das m�dias no teste-t
# e tamb�m graficamente representado.
  
  
## Comparando suas conclus�es anteriores, � poss�vel afirmar qual
## das duas vari�veis possui maior impacto no desenvolvimento economico?
## Por que? 

# Eu diria que o tipo do regime, j� que o p-valor � menor.
# Ambos influenciam e talvez ambos pudessem ser considerados num modelo,
# mas acredito que se fosse escolher uma, seria o tipo de regime.

##########################################################################

## Exerc�cio te�rico
## Levando em considera��o as vari�veis de seu trabalho final,
## qual dos 3 testes estat�sticos utilizados seria adequado utilizar?

# De correla��o; j� que a VD (n�mero de secretarias)
# e a VI (fragmenta��o partid�ria) s�o cont�nuas.