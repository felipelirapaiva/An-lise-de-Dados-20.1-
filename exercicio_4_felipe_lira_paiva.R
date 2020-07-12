# Exercício 4 - Felipe Lira Paiva


## Faça todos os gráficos utilizando um tema que você ache mais adequado
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

      
## Observe o banco de dados com as funções adequadas

      glimpse(banco) # Nº de linhas e colunas; 1ºs casos; tipos; nomes. 
      str(banco) # Nº de linhas e colunas; 1ºs casos; tipos
      summary(banco) # variáveis (min, max, média, 1st e 3rd quadrante...)
      head(banco) # primeiras linhas
      tail(banco) # últimas linhas

## A variável democ_regime08 indica se um país é democrático.
## Usando as ferramentas de manipulacao de bancos de dados, verifique
## quantos paises sao democraticos ou nao, e apresente esta variável 
## graficamente

      banco %>%
        count(democ_regime08)

## 95 democráticos; 69 não democráticos; 3 sem dados

      ggplot(banco, aes(fct_infreq(democ_regime08)))+
        geom_bar()+
        theme_classic()+
        labs(title = "Figura 1",
           subtitle = "Gráfico de países democráticos e não democráticos",
           x = "O país é democrático?",
           y = "Número",
           caption = "Elaboração própria a partir do banco 'World' (R)")
      
      
## Teste a relação entre a variável democ_regime08 e a variável
## muslim (que indica se um país é muçulmano ou não). E represente
## visualmente as variáveis para visualizar se esta religião
## aumenta ou diminui a chance de um país ser democrático
## Qual seria sua conclusão com relação a associação destas duas
## variáveis?

      
# Como são duas categóricas, tem que fazer um chi-quadrado
# Primeiro, a tabela com o número total (table(x,y))
      tabela1 <- table(banco$muslim, banco$democ_regime08)
 
# Segundo, a tabela com a proporção em relação à coluna
      prop.table(tabela1,2)

# Terceiro, o Chi-quadrado
      chisq.test(tabela1)

# Mosaic plot (topo = muslim / lado = democrático)
      mosaicplot(tabela1, shade = TRUE)
      
# Associação (topo = democrático / lado = muslim)
      assoc(tabela1, shade = TRUE)
      
# Gráfico de barra
      ggplot(banco, aes(muslim, fill = democ_regime08)) +
        geom_bar(position = "fill")+
        theme_classic()+
        labs(title = "Figura 2",
             subtitle = "Gráfico de países democráticos e não democráticos",
             x = "O país é muçulmano",
             y = "Número",
             caption = "Elaboração própria a partir do banco 'World' (R)") 
      
# A partir dos dados acima, podemos concluir que existe uma relação
# positiva entre ser muçulmano e não democrático, a nível de país;
# Uma associação negativa entre ser muçulmano e democrático, outra
# negativa entre não ser muçulmano e não ser democrático.
      
      
## A variável gdppcap08 possui informação sobre o PIB per capta
## dos países. Faça uma representação gráfica desta variável

      ggplot(banco, aes(gdppcap08, ..count../sum(..count..)))+
        geom_density()+
        scale_y_continuous(labels = percent)+
        theme_classic()+
        labs(title = "Figura 3",
             subtitle = "Gráfico de distribuição de PIB per capita",
             x = "PIB per capita",
             y = "Porcentagem de países",
             caption = "Elaboração própria a partir do banco 'World' (R)") 
      


## Faça um sumario com a média, mediana e desvio padrão do pib per capta
## para cada tipo de regime politico, represente a associação destas
## variáveis graficamente, e faça o teste estatístico adequado para
## chegar a uma conclusão. Existe associaçào entre as variáveis?

# (1.1) O sumário sem NA:
      banco %>%
        filter(!is.na(democ_regime08),
               !is.na(gdppcap08)) %>%
             group_by(democ_regime08) %>%
        summarise(media = mean(gdppcap08, na.rm = TRUE),
                  mediana = median(gdppcap08, na.rm = TRUE),
                  desvio = sd(gdppcap08, na.rm = TRUE))
      
# (1.2) O sumário com NA:
      banco %>%
        group_by(democ_regime08) %>%
        summarise(media = mean(gdppcap08, na.rm = TRUE),
                  mediana = median(gdppcap08, na.rm = TRUE),
                  desvio = sd(gdppcap08, na.rm = TRUE))
    
# (2.1) Associação graficamente: com boxplot
      ggplot(banco, aes (democ_regime08, gdppcap08))+
        geom_boxplot()+
        theme_classic()+
        labs(title = "Figura 4",
             subtitle = "Boxplot de PIB per capita por país",
             x = "O país é democrático?",
             y = "PIB per capita",
             caption = "Elaboração própria a partir do banco 'World' (R)") 

# (2.2) Associação graficamente: com densidade      
      ggplot(banco, aes (gdppcap08, fill = democ_regime08))+
        geom_density(alpha = 0.5)+
        theme_classic()+
        scale_y_continuous(labels = percent)+
        labs(title = "Figura 5",
             subtitle = "Regime democrático e PIB per capita",
             x = "PIB per capita",
             y = "Porcentagem de países",
             caption = "Elaboração própria a partir do banco 'World' (R)") 
      
# (3) Teste-T
      t.test(gdppcap08 ~ democ_regime08, data = banco)
      
# A partir dos dados acima (Teste-T, gráficos), podemos dizer que sim,
# há uma relação (ainda que não queira dizer causalidade) entre PIB per
# capita e democracia: tanto a mediana quanto os valores da democracia
# são levemente maiores. No Teste-T, a diferença das médias não é 
# igual a 0 e o p-valor é baixo (0,004). Acredito que podemos concluir
# que é estatisticamente significante.
# Atenção para a existência de outliers que podem, possivelmente,
# representar monarquias ricas em recursos naturais e "pequenas".
# Num trabalho, talvez fosse interessante retirá-los e testar sem eles.
      
      
      
## Por fim, ao invés de utilizar uma variável binária de democracia,
## utilize a variável dem_score14 para avaliar se existe uma associação
## entre regime político e desenvolvimento econômico. Represente
## a associação graficamente, faça o teste estatístico e explica sua
## conclusão

# (1) Gráfico
      ggplot(banco, aes(dem_score14, gdppcap08))+
        geom_point()+
        theme_classic()+
        labs(title = "Figura 6",
             subtitle = "Gráfico de valor democrático e PIB per capita",
             x = "Valor da democracia",
             y = "PIB per capita",
             caption = "Elaboração própria a partir do banco 'World' (R)") 
      
# (2) Correlação
      cor.test(banco$dem_score14, banco$gdppcap08)
      

# Existe uma correlação moderada (50.5), o que não é desprezível,
# O p-valor é baixo, então o teste é estatisticamente significante.
# Existem outliers (pouco valor de democracia com PIBs muito altos),
# como vimos na questão anterior também.
      
      
## Teste a associação entre renda perca capta e religiao (com a variável
## muslim) e represente graficamente. Qual é sua conclusão? 

# (1) Associação com o teste-t
  
  t.test(gdppcap08 ~ muslim, data = banco)
    
# (2.1) Associação graficamente: com boxplot
  ggplot(banco, aes (muslim, gdppcap08))+
    geom_boxplot()+
    theme_classic()+
    labs(title = "Figura 7",
         subtitle = "Boxplot de PIB per capita por país, se mulçumano",
         x = "O país é muçulmano?",
         y = "PIB per capita",
         caption = "Elaboração própria a partir do banco 'World' (R)") 
  
# (2.2) Associação graficamente: com densidade      
  ggplot(banco, aes (gdppcap08, fill = muslim))+
    geom_density(alpha = 0.5)+
    theme_classic()+
    scale_y_continuous(labels = percent)+
    labs(title = "Figura 8",
         subtitle = "Religião muçulmana e PIB per capita",
         x = "PIB per capita",
         y = "Porcentagem de países",
         caption = "Elaboração própria a partir do banco 'World' (R)") 
  
# Sim, existe uma relação. Os países não muçulmanos possuem,
# em geral, uma renda maior que os países muçulmanos.
# É possível ver isso a partir da diferença das médias no teste-t
# e também graficamente representado.
  
  
## Comparando suas conclusões anteriores, é possível afirmar qual
## das duas variáveis possui maior impacto no desenvolvimento economico?
## Por que? 

# Eu diria que o tipo do regime, já que o p-valor é menor.
# Ambos influenciam e talvez ambos pudessem ser considerados num modelo,
# mas acredito que se fosse escolher uma, seria o tipo de regime.

##########################################################################

## Exercício teórico
## Levando em consideração as variáveis de seu trabalho final,
## qual dos 3 testes estatísticos utilizados seria adequado utilizar?

# De correlação; já que a VD (número de secretarias)
# e a VI (fragmentação partidária) são contínuas.