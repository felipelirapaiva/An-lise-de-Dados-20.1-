Exercicio 10 - Análise de Dados
================
Felipe Lira Paiva

Continuaremos com a utilização dos dados do ESEB2018. Carregue o banco
da mesma forma que nos exercicios anteriores

``` r
library(tidyverse)
library(haven)

link <- "https://github.com/MartinsRodrigo/Analise-de-dados/blob/master/04622.sav?raw=true"

download.file(link, "04622.sav", mode = "wb")

banco <- read_spss("04622.sav") 

banco <- banco %>%
  mutate(D10 = as_factor(D10)) %>%
  filter(Q18 < 11,
         D9 < 9999998,
         Q1501 < 11,
         Q12P2_B < 3) %>%
  mutate(Q12P2_B = case_when(Q12P2_B == 1 ~ 0,  # Quem votou em Haddad = 0
                             Q12P2_B == 2 ~ 1)) # Quem votou em Bolsonaro = 1
```

Crie a mesma variável de religião utilizada no exercício anterior

``` r
Outras <- levels(banco$D10)[-c(3,5,13)]

banco <- banco %>%
  mutate(religiao = case_when(D10 %in% Outras ~ "Outras",
                              D10 == "Católica" ~ "Católica",
                              D10 == "Evangélica" ~ "Evangélica",
                              D10 == "Não tem religião" ~ "Não tem religião"))
```

#### Questão 1

Faça uma regressão linear utilizando as mesmas variáveis do exercício 9:
idade(D1A\_ID), educação (D3\_ESCOLA), renda (D9), nota atribuída ao PT
(Q1501), auto-atribuição ideológica (Q18), sexo (D2\_SEXO) e religião
(variável criada no passo anterior) - explicam o voto em Bolsonaro
(Q12P2\_B).

``` r
reg1 <- lm(Q12P2_B ~ D1A_ID + D3_ESCOLA + D9 + Q1501 + Q18 + D2_SEXO + religiao, data = banco)

summary(reg1)
```

    ## 
    ## Call:
    ## lm(formula = Q12P2_B ~ D1A_ID + D3_ESCOLA + D9 + Q1501 + Q18 + 
    ##     D2_SEXO + religiao, data = banco)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.05532 -0.19854  0.01565  0.16182  0.96682 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               7.067e-01  6.469e-02  10.924  < 2e-16 ***
    ## D1A_ID                    1.140e-03  7.539e-04   1.512  0.13074    
    ## D3_ESCOLA                 5.547e-03  5.226e-03   1.061  0.28873    
    ## D9                       -9.837e-07  3.196e-06  -0.308  0.75832    
    ## Q1501                    -7.728e-02  2.799e-03 -27.610  < 2e-16 ***
    ## Q18                       2.651e-02  3.093e-03   8.570  < 2e-16 ***
    ## D2_SEXO                  -5.286e-02  2.089e-02  -2.530  0.01154 *  
    ## religiaoEvangélica        7.684e-02  2.363e-02   3.251  0.00118 ** 
    ## religiaoNão tem religião -2.746e-03  4.238e-02  -0.065  0.94835    
    ## religiaoOutras           -7.263e-02  3.678e-02  -1.975  0.04855 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3489 on 1138 degrees of freedom
    ## Multiple R-squared:  0.5028, Adjusted R-squared:  0.4989 
    ## F-statistic: 127.9 on 9 and 1138 DF,  p-value: < 2.2e-16

#### Questão 2

Interprete o resultado dos coeficientes

RESPOSTA: O valor da constante (ou intercepto, valor de Y quando as VIs
forem 0) foi de 0,7067 (p-valor muito baixo).

1.  A idade (`D1A_ID`) teve uma relção positiva e um coeficiente de
    0.00114 (p-valor não significativo);
2.  A educação (`D3_ESCOLA`) teve uma relação também positiva e um
    coeficiente estimado em 0.005547 (p-valor não significativo a 0.05);
3.  A renda (`D9`) teve uma relação negativa (ou seja, quanto maior a
    renda, “menos” o voto em Bolsonaro), porém foi o menor coeficiente
    (-9.837e-07) e um p-valor bastante alto (ou seja, não é
    estatisticamente significativo);
4.  A nota atribuída ao PT (`Q1501`), como bem poderíamos esperar, tem
    uma relação negativa (afinal, maior apreço ao PT leva as pessoas a
    votarem menos em Bolsonaro): o coeficiente é de -0,07728 e o p-valor
    é bastante baixo (ou seja, é estatisticamente significativo);
5.  A auto-atribuição ideológica (`Q18`) também é estatisticamente
    significativa (p-valor muito baixo). O coeficiente tem uma direção
    positiva (mais a direita, mais vota em Bolsonaro) e o valor é
    0,02651;
6.  A variável de sexo (`D2_SEXO`) tem um coeficiente com relação
    negativa (lembrando que o gênero que está aparecendo é o feminino,
    então interpretamos que: o fato de ser mulher diminui o voto em
    Bolsonaro) no valor de -0,05286 e é estatisticamente significante
    (p-valor \< 0.05);
7.  Na variável de `religião` encontramos que: ser evangélico está
    associado positivamente (coeficiente: 0,07684 e p-valor \< 0.05); as
    categorias de não ter religião ou ter outras religiões estão
    negativamente associados com o voto em Bolsonaro (coeficientes de
    -0.002746 e 0.07263, respectivamente), mas apenas o p-valor de
    outras religiões é menor que 0.05 (portanto, não ter religão não é
    estasticamente significante). Lembramos que, aqui, a categoria de
    referência é “católicos”, ou seja, estamos falando do coeficiente e
    do p-valor em comparação com o grupo de católicos.
8.  Demais informações da regressão: erro residual padrão de 0.3489,
    r-quadrado de 0.50 e r-quadrado ajustado aproximadamente de de 0.50
    também.

#### Questão 3

O resultado difere dos encontrados anteriormente, quando a variavel
dependente era a aprovação de Bolsonaro?

RESPOSTA:

1.  A variável idade (`D1A_ID`) não apresentou grandes mudanças:
    permaneceu a mesma direção, o efeito diminuiu (no exercício 10 temos
    uma casa decimal a mais em relação ao exercício 9) e o p-valor
    manteve-se não significativo para menos que 0.05.
2.  A variável de educação (`D3_ESCOLA`) apresentou uma grande mudança.
    No exercício anterior, uma maior escolaridade estava associada a uma
    menor aprovação de Bolsonaro. Neste, a direção mudou: uma maior
    escolaridade está associada ao voto em Bolsonaro. Entretanto, ainda
    que a direção tenha mudado, o p-valor no exercício 10 não é
    estatisticamente significativo (no exercício 9 era menor que 0.05)
3.  A variável de renda (`D9`) manteve a mesma direção, mas teve um
    coeficiente maior no exercício 10. O p-valor, porém, não foi
    significativo nas duas ocasiões.
4.  A nota atribuída ao PT (`Q1501`) teve a mesma direção (negativa) nas
    duas regressões. O coeficiente era maior (havia uma casa decimal a
    menos) no exercício 9 em relação ao atual. O p-valor, nos dois
    casos, foi bastante baixo, mostrando que foi estatisticamente
    significativo.
5.  A auto-avaliação ideológica (`Q18`) teve a mesma direção nas duas
    regressões (relação positiva, quanto mais à direita, “maior” o apoio
    ou o voto em Bolsonaro). O coeficiente foi menor no exercício atual
    (uma casa decimal a menos em relação ao anterior) e o p-valor, nos
    dois, foi extremamente baixo.
6.  A variável de sexo (`D2_SEXO`) também teve a mesma direção
    (negativa) e teve um coeficiente menor no exercício atual (diferença
    de uma casa decimal). Em ambos os casos, o p-valor foi
    estatisticamente significante.
7.  A variável de `religião`, no exercício anterior, não foi
    estatisticamente significante para um p-valor menor que 0.05 para os
    três grupos (evangélicos, sem religião e outras religiões) em
    relação aos católicos. No exercício atual, tanto o grupo
    evangélico quanto o de outras religiões foram estatisticamente
    significativos. Além disso, todos os coeficientes diminuíram e “não
    ter religião” teve a direção trocada (era postivo e está negativo no
    exercício 10).
8.  Em linhas gerais, a primeira diferença que chama a atenção é que os
    coeficientes do presente exercício estão menores em relação aos do
    exercício passado. Além disso, uma variável (escolaridade) e um
    grupo (sem religião) sofreram uma grande mudança: a direção da
    relação. Nenhuma das duas, porém, foi estatisticamente
    significante.

#### Questão 4

Faça uma regressão logistica com as mesmas variáveis

``` r
reg2 <- glm (Q12P2_B ~ D1A_ID + D3_ESCOLA + D9 + Q1501 + Q18 + D2_SEXO + religiao, data = banco, family = binomial)

summary(reg2)
```

    ## 
    ## Call:
    ## glm(formula = Q12P2_B ~ D1A_ID + D3_ESCOLA + D9 + Q1501 + Q18 + 
    ##     D2_SEXO + religiao, family = binomial, data = banco)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.7529  -0.5625   0.2518   0.4744   2.5830  
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)               8.209e-01  5.298e-01   1.550  0.12124    
    ## D1A_ID                    1.001e-02  6.337e-03   1.580  0.11405    
    ## D3_ESCOLA                 5.634e-02  4.358e-02   1.293  0.19602    
    ## D9                       -4.635e-06  2.396e-05  -0.193  0.84660    
    ## Q1501                    -4.678e-01  2.666e-02 -17.545  < 2e-16 ***
    ## Q18                       2.242e-01  2.748e-02   8.159 3.37e-16 ***
    ## D2_SEXO                  -4.497e-01  1.739e-01  -2.586  0.00971 ** 
    ## religiaoEvangélica        6.217e-01  1.985e-01   3.132  0.00173 ** 
    ## religiaoNão tem religião -2.106e-02  3.478e-01  -0.061  0.95172    
    ## religiaoOutras           -6.736e-01  3.122e-01  -2.158  0.03096 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1557.84  on 1147  degrees of freedom
    ## Residual deviance:  862.45  on 1138  degrees of freedom
    ## AIC: 882.45
    ## 
    ## Number of Fisher Scoring iterations: 5

#### Questão 5

Transforme os coeficientes estimados em probabilidade

``` r
library(margins)


margins(reg2)
```

    ##    D1A_ID D3_ESCOLA         D9    Q1501     Q18 D2_SEXO religiaoEvangélica
    ##  0.001171  0.006589 -5.421e-07 -0.05471 0.02622 -0.0526            0.07346
    ##  religiaoNão tem religião religiaoOutras
    ##                 -0.002521       -0.08172

``` r
summary(margins(reg2))
```

    ##                    factor     AME     SE        z      p   lower   upper
    ##                    D1A_ID  0.0012 0.0007   1.5849 0.1130 -0.0003  0.0026
    ##                   D2_SEXO -0.0526 0.0202  -2.6078 0.0091 -0.0921 -0.0131
    ##                 D3_ESCOLA  0.0066 0.0051   1.2949 0.1953 -0.0034  0.0166
    ##                        D9 -0.0000 0.0000  -0.1935 0.8466 -0.0000  0.0000
    ##                     Q1501 -0.0547 0.0009 -57.9079 0.0000 -0.0566 -0.0529
    ##                       Q18  0.0262 0.0030   8.8434 0.0000  0.0204  0.0320
    ##        religiaoEvangélica  0.0735 0.0235   3.1280 0.0018  0.0274  0.1195
    ##  religiaoNão tem religião -0.0025 0.0417  -0.0605 0.9517 -0.0842  0.0791
    ##            religiaoOutras -0.0817 0.0379  -2.1574 0.0310 -0.1560 -0.0075

Interpretação:

1.  A idade (`D1A_ID`) tem um efeito marginal médio de 0.0012, isso
    significa que a cada ano de idade a mais, a chance de a pessoa votar
    em Bolsonaro é aumentada em 0.0012 (ou 0.12%). Obervamos que o
    p-valor não é significativo.
2.  A variável de gênero (`D2_SEXO`) tem uma relação negativa, o que
    significa que ser mulher diminui a chance de votar em Bolsonaro em
    0.0526 (ou menos 5.47% de probabilidade). O p-valor é
    estatisticamente sginificativo (p-valor \< 0.05).
3.  A educação (`D3_ESCOLA`) possui uma relação positiva, mas pequena:
    quanto maior a escolaridade, maior a chance da pessoa votar em
    Bolsonaro (0.0066 ou 0.66%). O p-valor, porém, não é significativo
    para 0.05.
4.  A renda (`D9`), por sua vez, aponta uma relação negativa (pelo
    sinal), mas não é possível ver, pois só há 4 zeros nas casas
    decimais (0.0000). Provavelmente, o efeito é demasiado pequeno e
    está em casas decimais menores. O p-valor não é significativo.
5.  A nota atribuída ao PT (`Q1501`) tem uma relação negativa: a cada
    ponto a mais de apreço ao PT, a probabilidade da pessoa votar em
    Bolsonaro diminui em 0.0547 (ou 5.47%). O p-valor é extremamente
    baixo.
6.  A auto-atribuição ideológica (`Q18`) possui uma relação positiva: a
    cada ponto na escala ideológica (lembrando que a esquerda está nos
    valores mais baixos e a direita nos mais altos), a chance de votar
    em Bolsonaro é aumentada em 0.0262 (ou 2.62%). O p-valor é
    extremamente baixo.
7.  Com variável de `religião`, por fim, temos o seguinte: (a) ser
    Evangélico aumenta a chance de votar em Bolsonaro em 0.0735 (7.35%;
    p-valor significativo); (b) não ter religião diminui em 0.0025
    (-0.25%; p-valor não significativo); (c) ter outras religiões
    diminui em 0.0817 (-8.17%, p-valor significativo). Lembramos que a
    categoria de referência são os católicos. Isso signifca que estamos
    comparando esses grupos em relação aos católicos.

#### Questão 6

Quais foram as diferenças no resultado entre usar a regressão linear e a
logistica?

RESPOSTA:

1.  Significância: Na linear e na logística, apenas a nota atribuída ao
    PT, a auto-atribuição ideológica, o gênero e os grupos evangélico e
    “outras religiões” foram estatisticamente significantes. Ou seja,
    esse é um aspecto de semelhança entre os dois modelos (não
    necessariamente isso deve ocorrer sempre, essas observações são
    relevantes para este exercício e as variáveis em questão)
2.  Religiões: pela regressão linear, poderíamos dizer que o grupo com
    maior beta é o de “Evangélicos”, porém, com a regressão logística
    observamos que, na verdade, é o de “outras religiões” (-8.17%
    vs. -7.35%). Decerto que os valores são próximos, mas é
    interessante notar essa mudança.
3.  Pode ser uma observação óbvia, mas parece-me mais confiável usar a
    logística, pois é muito intuitivo interpretar em quantos porcento
    ter tal característica aumenta ou diminui a chance de, neste caso,
    ter votado ou não em Bolsonaro.

#### Questão 7

Verifique a quantidade de classificações corretas da regressao logistica
e avalie o resultado

``` r
library(InformationValue)

predicted_prob <- predict(reg2, type = "response")


1 - misClassError(banco$Q12P2_B, 
                  predicted_prob, 
                  threshold = 0.5)
```

    ## [1] 0.8301

``` r
opt_cutoff <- optimalCutoff(banco$Q12P2_B, 
                            predicted_prob)

confusionMatrix(banco$Q12P2_B, 
              predicted_prob, 
              threshold = opt_cutoff)
```

    ##     0   1
    ## 0 393 105
    ## 1  83 567

``` r
prop.table(confusionMatrix(banco$Q12P2_B, 
                predicted_prob, 
                threshold = opt_cutoff))
```

    ##            0          1
    ## 0 0.34233449 0.09146341
    ## 1 0.07229965 0.49390244

RESPOSTA:

A quantidade de classificações corretas é 83,01%. Isso quer dizer que, a
partir da regressão logística o banco de dados foi “analisado” e a nossa
regressão acertaria a previsão de 83,01% dos casos.

É interessante notar a distribuição entre os acertos positivos e
negativos. O 0-0 (True-Positive) significa que o modelo acertou o “sim”
(aqui, voto). Isso ocorreu em 393 casos (ou 34,23%). O modelo também
acertou muitos negativos (“não”, ou seja, o não voto): foram 567 casos
(ou 49,39%). Os erros são de Falso Positivo (105 ocorrências e 9,14%0 e
de Falso Negativo (apenas 83 ocorrências ou 7,22%)

Eu diria que, no geral, o modelo é satisfatoriamente bom, pois, ainda
que não tenha conseguido prever perto de 100%, conseguiu prever 83%, o
que já é alto.
