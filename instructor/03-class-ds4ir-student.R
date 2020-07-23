# ---
#   title: "DS4IR"
# subtitle: "Análise Exploratória"
# author: 
#   - Professor Davi Moreira
# - Professor Rafael Magalhães
# date: "`r format(Sys.time(), '%d-%m-%Y')`"
# output: 
#   revealjs::revealjs_presentation:
#   theme: simple
# highlight: haddock
# transition: slide
# center: true
# css: stylesheet.css
# reveal_options:
#   controls: false  # Desativar botões de navegação no slide
# mouseWheel: true # Passar slides com o mous e
# ---
  
#```{r echo=FALSE, message=FALSE, warning=FALSE}
# devtools::install_github("kosukeimai/qss-package")
library("tidyverse")
library("here")
library("vcd")
library("ggcorrplot")
#```
#
# ##
# 
# <center>
#   > “In God we trust; all others must bring data.
# >
#   > --- William Edwards Deming
# 
# </center>
#   
#   ## Programa
#   
#   - Medidas de posição
# - Medidas de dispersão
# - Tabelas de contingência
# - Resumos gráficos
# - Interpretação de estatísticas descritivas
# 
# ## Motivação: Civilian Victimization during Wartime
# 
# - Após os ataques de 11 de setembro, os EUA e aliados invadiram o Afeganistão com o 
# objetivo de distruir a al-Qaeda. 
# 
# - Em 2003 a OTAN passa a se envolver no conflito com
# o envio de tropas de segurança (International Security Assistance Force - ISAF). 
# 
# - Entre 
# outras ações, a ISAF se engajou numa campanha de assistência econômica para ganhar apoio civil. 
# 
# ## Motivação: Civilian Victimization during Wartime
# 
# - Para avaliar o sucesso dessa campanha era essencial mensurar a opinião dos civis 
# durante a guerra. 
# 
# - Realizar esse tipo de consulta à população em meio a um conflito armado
# não é trivial. Além de dificuldades operacionais para pesquisa, a população pode omitir e dificultar a 
# coleta de informações. 
# 
# - Um grupo de cientistas enfrentou esse desafio e é 
# o resultado desse trabalho que vamos utilizar em aula.
# 
# ## Motivação: referência
# 
# [Lyall, Jason, Graeme Blair, and Kosuke Imai. 2013. “Explaining Support for Combatants during Wartime: A Survey Experiment in Afghanistan.” American Political Science Review 107(4): 679–705.](https://www.cambridge.org/core/journals/american-political-science-review/article/explaining-support-for-combatants-during-wartime-a-survey-experiment-in-afghanistan/B0E55BA87D4EBF66F0BF6135959541A7)
# 
# [Blair, Graeme, Kosuke Imai, and Jason Lyall. 2014. “Comparing and Combining List and Endorsement Experiments: Evidence from Afghanistan.” American Journal of Political Science 58(4): 1043–63.](https://onlinelibrary.wiley.com/doi/abs/10.1111/ajps.12086)
# 
# ## Importando dados
# 
#  ```{r message=FALSE, warning=FALSE}
afghan <- read_csv2(here("data", "afghan.csv"))
# ```
# 
# Vamos consultar nossa base de dados!
#   
#   ## Medidas de posição
#   
#   ```{r eval = FALSE, message=FALSE, warning=FALSE}
# Máximo
max(afghan$educ.years) 

# Mínimo
min(afghan$educ.years)

# Média Aritmética
mean(afghan$educ.years)

# Mediana
median(afghan$educ.years)
# ```
# 
# ## Medidas de posição
# 
# ```{r eval = FALSE, message=FALSE, warning=FALSE}
# Quantis
summary(afghan$educ.years)

# Percentis / Decis...
quantile(afghan$educ.years, probs = seq(0,1, .25))

# ```
# 
# ## Medidas de posição
# 
# Já vimos uma função que permite obter as principais medidas de posição de uma só vez. 
# Com ela, não precisamos consultar uma a uma para descrever nossos dados:
#   
#   ```{r eval = FALSE, message=FALSE, warning=FALSE}

afghan %>%
  select(age, educ.years, employed, income) %>%
  summary()

# ```
# 
# ## Medidas de posição
# 
# Por padrão, carregar a base de dados com a função `read_csv()` não transforma strings 
# em fatores. Podemos usar a função `count()` para ter um resumo dos diferentes níveis da 
# variável `income`.
# 
# ```{r eval = FALSE, message=FALSE, warning=FALSE}

count(afghan, income)

# ```
# 
# ## Tabelas de contingência
# 
# **Survey question**
#   
#   "Over the past year, have you or anyone in your family suffered harm due to the 
# actions of the Foreign Forces / the Taliban?"
# 
# ```{r eval = FALSE, message=FALSE, warning=FALSE}

afghan %>%
  filter(!is.na(violent.exp.ISAF), !is.na(violent.exp.taliban)) %>%
  group_by(violent.exp.ISAF, violent.exp.taliban) %>%
  count() %>%
  ungroup() %>%
  mutate(prop = n / sum(n)) 

# ```
# 
# ## Mosaic Plot
# 
# ```{r eval=TRUE, echo=FALSE, message=FALSE, warning=FALSE, fig.width=6, fig.height=4}
library("vcd")
mosaic(~ violent.exp.ISAF + violent.exp.taliban, data = afghan)
# ```
# 
# [Using Graphs Instead of Tables in Political Science.](https://www.cambridge.org/core/journals/perspectives-on-politics/article/using-graphs-instead-of-tables-in-political-science/9FD63E9EE686AF046732191EE8A68034#:~:text=When%20political%20scientists%20present%20empirical,draw%20clear%20and%20correct%20inferences.)
#                                                       
## Medidas de dispersão
#
# ```{r, results = 'asis', eval = FALSE}
# Amplitude
max(afghan$educ.years) - min(afghan$educ.years)
# ou
range(afghan$educ.years)[2] - range(afghan$educ.years)[1]

# Variância
var(afghan$educ.years)

# Desvio padrão
sd(afghan$educ.years)
# ou
afghan %>% select(educ.years) %>% var %>% sqrt
# ```
#
## Exercicio
#
#A base de dados a seguir apresenta o desempenho de duas turmas numa prova de matemática.
#Analise os dados e indique a principal diferença entre elas.

#```{r, results = 'asis', eval = FALSE}
mat <- tibble(T1 = c(7,3,5,6,4), 
              T2 = c(rep(5, 5)))
#```
#
## Exercicio: resposta
# 
## Visualizando a distribuição univariada - Barplot
# 
# ```{r echo = FALSE, message=FALSE, warning=FALSE}
# 
afghan %>%
 mutate(violent.exp.ISAF.fct =
          fct_explicit_na(fct_recode(factor(violent.exp.ISAF),
                                     Harm = "1", "No Harm" = "0"),
                          "No response")) %>% 
 ggplot(aes(x = violent.exp.ISAF.fct, y = ..prop.., group = 1)) +
 geom_bar() +
 xlab("Response category") +
 ylab("Proportion of respondents") +
 ggtitle("Civilian Victimization by the ISAF")
# 
# ```
# 
# ## Visualizando a distribuição univariada - Histogram
# 
# ```{r echo = FALSE, message=FALSE, warning=FALSE}
# 
ggplot(afghan, aes(x = age, y = ..density..)) +
 geom_histogram(binwidth = 5, boundary = 0) +
 scale_x_continuous(breaks = seq(20, 80, by = 10)) +
 labs(title = "Distribution of respondent's age",
      y = "Density", x = "Age")
# 
# ```
# 
# ## Visualizando a distribuição univariada - Histogram
# 
# ```{r echo = FALSE, message=FALSE, warning=FALSE}
# 
ggplot(afghan, aes(x = educ.years, y = ..density..)) +
 geom_histogram(binwidth = 1, center = 0) +
 geom_vline(xintercept = median(afghan$educ.years),
            color = "blue", size = 1) +
 annotate("text", x = median(afghan$educ.years),
          y = 0.2, label = "median", hjust = -0.1) +
 labs(title = "Distribution of respondent's education",
      x = "Years of education",
      y = "Density")
# 
# ```
# 
## Visualizando a distribuição univariada - Boxplot
# 
# ```{r echo=FALSE, message=FALSE, warning=FALSE}
# 
ggplot(afghan, aes(x = 1, y = age)) +
 geom_boxplot() +
 coord_flip() +
 labs(y = "Age", x = "", title = "Distribution of Age")
# 
# ```
# 
# ## Visualizando a distribuição univariada - Boxplot
# 
# ```{r echo=FALSE, message=FALSE, warning=FALSE}
# 
ggplot(afghan, aes(y = educ.years, x = province)) +
 geom_boxplot() +
 labs(x = "Province", y = "Years of education",
      title = "Education by Province")
# 
# ```
# 
# 
# ## Visualizando a distribuição univariada - Violin plot
# 
# ```{r echo=FALSE, message=FALSE, warning=FALSE}
# 
ggplot(afghan, aes(y = educ.years, x = province)) +
 geom_violin() +
 #  coord_flip() +
 labs(x = "Province", y = "Years of education",
      title = "Education by Province")
# 
# ```
# 
# ## Covariação
# 
# ## Motivação: Medindo a polarização política
# 
# - Cientistas socias utilizam modelos de mensuração para sumarizar e entender comportamentos,
# atitudes e padrões de atividade humana. 
# 
# - O exemplo mais proeminente de mensuração na ciência 
# política é a polarização resultante do padrão de votação nominal dos congressistas. 
# 
# - Nessa parte da aula vamos nos dedicar à análise dos pontos ideais dos congressistas americanos 
# estimada pelo modelo DW-NOMINATE.
# 
# **Referência**: [Polarized America: The Dance of Ideology and Unequal Riches](https://mitpress.mit.edu/books/polarized-america-second-edition)
# 
# ## Importando dados
# 
# ```{r message=FALSE, warning=FALSE}
congress_us <- read_csv2(here("data", "congress.csv"))
# ```
# 
# Vamos consultar nossa base de dados!
#  
#  ## Gráfico de dispersão
#  
#  Pontos ideais dos congressistas americanos nas legislaturas 80 (1947-1949) 
# e 112 (2011-2013) em duas dimensões: *economic and racial*.
# 
# ```{r echo=FALSE, message=FALSE, warning=FALSE, fig.width=7, fig.height=5}
congress_us %>%
 filter(congress %in% c(80, 112),
        party %in% c("Democrat", "Republican")) %>%
 ggplot(aes(x = dwnom1, y = dwnom2, colour = party)) +
 geom_point() +
 facet_wrap(~ congress) +
 coord_fixed() +
 scale_y_continuous("racial liberalism/conservatism",
                    limits = c(-1.5, 1.5)) +
 scale_x_continuous("economic liberalism/conservatism",
                    limits = c(-1.5, 1.5))
# ```
# 
# ## Ilustração da importância do modelo
# 
# <center>
#  ![Ideal](images/ideal-point.png){width=600px}
# 
# [Quantitative Social Science](http://qss.princeton.press/)
# </center>
#  
#  ## Gráfico de linha - *Time Series Plot*
#  
#  Com os dados de votação por congressista, qual seria um boa medida descritiva para 
# analisar o padrão de votação dos partidos em uma dimensão específica? Por exemplo, 
# na dimensão econômica (`dwnom1`).
# 
# ## Gráfico de linha - *Time Series Plot*
# 
# ```{r echo=FALSE, message=FALSE, warning=FALSE}
congress_us %>% 
 group_by(congress, party) %>%
 summarise(dwnom1 = median(dwnom1)) %>%
 filter(party %in% c("Democrat", "Republican")) %>%
 ggplot(aes(x = congress, y = dwnom1,
            colour = fct_reorder2(party, congress, dwnom1))) +
 geom_line() +
 ylim(-1,1) +
 labs(y = "DW-NOMINATE score (1st Dimension)", x = "Congress",
      colour = "Party")
# ```
# 
## Exercício: 

# Reproduza a análise considerando agora a dimensão racial (`dwnom2`).

## Exercício: resposta

# ## Correlação
#  
#  <center>
#  ![Correl](images/correlation.png){width=800px}
# 
# [xkcd](https://xkcd.com/552/)
# </center>
#  
#  ## Correlação
#  
#  Retomando nosso exemplo:
#  
#  ```{r echo=FALSE, message=FALSE, warning=FALSE, fig.width=7, fig.height=5}
congress_us %>% 
 group_by(congress, party) %>%
 summarise(dwnom1 = median(dwnom1)) %>%
 filter(party %in% c("Democrat", "Republican")) %>%
 ggplot(aes(x = congress, y = dwnom1,
            colour = fct_reorder2(party, congress, dwnom1))) +
 geom_line() +
 ylim(-1,1) +
 labs(y = "DW-NOMINATE score (1st Dimension)", x = "Congress",
      colour = "Party")
# ```
# 
# ## Correlação
# 
# Calculando a polarização:
#  
#  ```{r echo=TRUE, message=FALSE, warning=FALSE}
party_polarization <-
 congress_us %>%
 group_by(congress, party) %>%
 summarise(dwnom1 = median(dwnom1)) %>%
 filter(party %in% c("Democrat", "Republican")) %>%
 spread(party, dwnom1) %>%
 mutate(polarization = Republican - Democrat)
# 
# ```
# 
# ## Exercício: 
#
# Desenvolva um gráfico que nos ajude a visualizar a variável criada.

## Exercício: resposta


### Correlação
# 
#  Questão: estaria a desigualdade econômica associada à polarização?
#  
#  <center>
#  ![Gini](images/gini.png){width=380px}
# 
# [wiki](https://en.wikipedia.org/wiki/Gini_coefficient)
# 
# O coeficiente de Gini é igual à área $A$ dividida pela soma das áreas $A+B$, $Gini = A/(A + B)$. 
# </center>
#  
#  ## Correlação
#  
#  Questão: estaria o aumento da desigualdade econômica associada à polarização?
#  
#  ```{r message=FALSE, warning=FALSE}

gini_us <- read_csv2(here("data", "gini_us.csv"))

# ```
#
## Correlação
#
# Questão: estaria o aumento da desigualdade econômica associada à polarização?
#  
#  ```{r echo=FALSE, message=FALSE, warning=FALSE, fig.width=6, fig.height=4}
gini_us %>% ggplot(aes(x = year, y = gini)) +
 geom_point() +
 geom_line() +
 labs(x = "Year", y = "Gini coefficient") +
 ggtitle("Income Inequality")
# 
# ```
# 
# ## Exercício:
# 
# Como uma legislatura dura 2 anos no caso americano, vamos usar o índice de Gini do 
# último ano de cada legislatura. Por isso, desenvolva código que realize essa tarefa e 
# aloque o resultado num vetor chamado `gini_congress`.
# 
# **Dica**: perceba que a base de dados está ou pode ser ordenada por ano. No pacote `tidyverse`
# a função `slice` pode ajudar nessa tarefa. Para mais delalhes sobre a função, veja: 
#  [slice](https://dplyr.tidyverse.org/reference/slice.html).

## Exercício: resposta


## Correlação
 
# A correlação entre $x$ e $y$ representa como, em média, duas variáveis se movem 
# juntas em relação às suas respectivas médias.
# 
# \begin{equation*}
# r_{xy}={\frac {1}{n-1}}\sum _{i=1}^{n}\left({\frac {x_{i}-{\bar {x}}}{s_{x}}}\right)\left({\frac {y_{i}-{\bar {y}}}{s_{y}}}\right)
# \end{equation*}
# 
# ```{r message=FALSE, warning=FALSE}
# correlação
cor(gini_congress$gini, party_polarization$polarization)
# ```
# 
# ## Correlação: matriz
# 
# ```{r eval=FALSE, message=FALSE, warning=FALSE}
mtcars %>% select(1:7)
# ```
# 
# ## Correlação: matriz
# 
# ```{r echo=FALSE, message=FALSE, warning=FALSE, fig.width=7, fig.height=5}
library("ggcorrplot")

mtcars %>% select(1:7) %>% cor() %>%
 ggcorrplot(hc.order = TRUE, type = "lower",
            outline.col = "white",
            ggtheme = ggplot2::theme_gray,
            colors = c("#6D9EC1", "white", "#E46726"))

# ```
# 
# Para mais detalhes sobre o uso do pacote e da função, não deixe de ver esse tutorial:
#  [Visualization of a correlation matrix using ggplot2](http://www.sthda.com/english/wiki/ggcorrplot-visualization-of-a-correlation-matrix-using-ggplot2)
# 
# ## Material adicional
# Além do livro e artigos mencionados, recomendamos:
#  
#  - [QSS Tidyverse Code](https://jrnold.github.io/qss-tidy/)
# - [Mosaic Plot](https://rpubs.com/tskam/mosaic_plot)
# - [Mosaic Plot e ggplot2](https://cran.r-project.org/web/packages/ggmosaic/vignettes/ggmosaic.html)
# 
# 
# ## Tarefa da aula
# 
# As instruções da tarefa estão no arquivo `NN-class-ds4ir-assignment.rmd` da pasta 
# `assignment` que se encontra na raiz desse projeto.
                                                     
                                                       
                                                       
                                                       
                                                       
                                                       
                                                       
                                                       
                                                       
                                                       
                                                       