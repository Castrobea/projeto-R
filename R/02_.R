### Instalando pacotes

install.packages("knitr",dependencias=T)
install.packages("rmarkdown",dependencias=T)

## Análise exploratória de dados

# Explorando os dados de Anscombe (Quarteto de Anscombe)

data("anscombe")
dim(anscombe) # dimensao dos dados, N de linhas e N de colunas
head(anscombe) # seis primeiras linhas dos dados
class(anscombe) # classe do objeto
str(anscombe) # estrutura do objeto

# Selecionando colunas para fazer a média

mean(anscombe$x1)
mean(anscombe$x2)
mean(anscombe$x3)
mean(anscombe$x4)

# O mesmo calculo de médias, agora usando apenas em 1 linha de comando

  # media de todos os vetores x
  apply(anscombe[,1:4], 2, mean) #aplica uma funcao a todas as linhas de um objeto

  # media de todos os vetores y
  apply(anscombe[,5:8], 2, mean)

# Variância dos dados
apply(anscombe, 2, var) # aplica a funcao var a todas as linhas do objeto

# Correlação entre as variáveis

cor(anscombe$x1, anscombe$y1)
cor(anscombe$x2, anscombe$y2)
cor(anscombe$x3, anscombe$y3)
cor(anscombe$x4, anscombe$y4)

# Coeficiente de regressão

  # Primeiro criamos objetos com as regressoes dos quatro conjuntos
  regressao1 <- lm(anscombe$y1 ~ anscombe$x1)
  regressao2 <- lm(anscombe$y2 ~ anscombe$x2)
  regressao3 <- lm(anscombe$y3 ~ anscombe$x3)
  regressao4 <- lm(anscombe$y4 ~ anscombe$x4)

  # Vamos criar agora uma lista com todos os modelos para facilitar o trabalho
  regressaolist <- list(regressao1, regressao2, regressao3, regressao4)

  # Agora podemos calcular de forma menos repetitiva os coeficientes de regressao
  lapply(regressaolist, coef)

#Em que os dados são diferentes?
  anscombe

# Funcao par para definir as configuracoes da janela grafica
  par(mfrow=c(2, 2), #abre uma janela gráfica com 2 linhas  e 2 colunas
      las=1, # deixa as legendas dos eixos na vertical
      bty="l") # tipo do box do grafico em L
  plot(anscombe$y1 ~ anscombe$x1) #plot das variaveis
  abline(regressaolist[[1]]) # adicionando a reta prevista pelo modelo de regressao
  plot(anscombe$y2 ~ anscombe$x2)
  abline(regressaolist[[2]])
  plot(anscombe$y3 ~ anscombe$x3)
  abline(regessaolist[[3]])
  plot(anscombe$y4 ~ anscombe$x4)
  abline(regressaolist[[4]])

  par(mfrow=c(1, 1)) # retorna a janela grafica para o padrao de 1 linha e 1 coluna

### Parte 2 da análise exploratória de dados

head(iris)
summary(iris)

# Verificando quantas informações temos por espécie
table(iris$Species)

#Verificando a média das variáveis por espécie

  # media do comprimento de sepala por especie
  tapply(X = iris$Sepal.Length, INDEX = list(iris$Species), FUN = mean)
  # a mesma tarefa, executada por outra funcao. Outros argumentos e outra saída
  aggregate(x = iris$Sepal.Length, by = list(iris$Species), FUN = mean)
  # ainda a mesma tarefa, com a mesma função mas em uma notação diferente
  aggregate(Sepal.Length ~ Species, data = iris, mean)

  # Fazendo o mesmo para as outras variáveis
  aggregate(Petal.Width ~ Species, data = iris, mean)
  aggregate(Sepal.Width ~ Species, data = iris, mean)
  aggregate(Petal.Length ~ Species, data = iris, mean)

#Calculando o desvio padrão das variáveis

tapply(X = iris$Sepal.Length, INDEX = list(iris$Species), FUN = sd)
tapply(X = iris$Sepal.Width, INDEX = list(iris$Species), FUN = sd)
tapply(X = iris$Petal.Length, INDEX = list(iris$Species), FUN = sd)
tapply(X = iris$Petal.Width, INDEX = list(iris$Species), FUN = sd)

# Calculando a média por espécie de todas as variáveis juntamente

  # criando matriz de 3 colunas - uma para cada sp - e 4 linhas - uma para cada     metrica
  medias <- matrix(NA, ncol = 3, nrow = 4)

  # definindo o nome das colunas e das linhas da matriz
  colnames(medias) <- unique(iris$Species)
  rownames(medias) <- names(iris)[-5]
  for (i in 1:4){
  medias[i,] <- tapply(iris[,i], iris$Species, mean)
  }

###Estatísticas descritivas


# Média
  vars <- iris[, -5]
  apply(vars, 2, mean)

# Mediana
  apply(vars, 2, median)

# Moda
  freq_sl <- sort(table(iris$Sepal.Length), decreasing = TRUE)
  freq_sl[1]

# Variância: desvio da média
  apply(vars, 2, var)

# Desvio padrão: raiz quadrada da variância
  sd01 <- apply(vars, 2, sd)
  # outra forma:
  sd02 <- apply(vars, 2, function(x) sqrt(var(x)))
  sd01
  sd02
  sd01 == sd02

# Coeficiente de variação: medida relativa de desvio padrão (criando uma função, pois o R não possui função para calcular o coeficiente de variação)
  cv <- function(x){
    sd(x)/mean(x)*100
  }
  apply(vars, 2, cv)

# Quartis ou percentis

  # sumario de 5 numeros
  apply(vars, 2, quantile)

  # 5%, 50% e 95%
  apply(vars, 2, quantile, probs=c(0.05, 0.5, 0.95))

# Intervalo: diferenca entre o menor e o maior valor

  # a funcao range nos retorna os valores minimo e maximo
  apply(vars, 2, range)

  # aplicando a funcao diff ao resultado do range, temos o valor desejado
  my_range <- function(x){
    diff(range(x))
  }
  apply(vars, 2, my_range)

# Intervalo interquartil: é a diferença entre o quartil superior (75%) e o quartil inferior (25%)
  apply(vars, 2, IQR)

# Correlação
  cor(vars)

### Métodos gráficos

# Gráficos de barra
  barplot(table(iris$Species))

# Histograma
  par(mfrow=c(2, 2))
  hist(iris$Sepal.Length)
  hist(iris$Sepal.Width)
  hist(iris$Petal.Length)
  hist(iris$Petal.Length)

  par(mfrow=c(1, 1))

  # Mostrando os intervalos
  par(mfrow=c(1, 2))
  hist(iris$Sepal.Width)
  hist(iris$Sepal.Width, breaks = 4)

  par(mfrow=c(1, 1))

# Curva de densidade
  par(mfrow=c(1, 2))
  hist(iris$Sepal.Width)
  hist(iris$Sepal.Width, freq = FALSE)

  par(mfrow=c(1, 1))

# Plot de curva de densidade
  par(mfrow=c(1, 2))

  plot(density(iris$Sepal.Width))

  # plot da curva de densidade sobre o histograma de densidade
  hist(iris$Sepal.Width, freq = FALSE)
  lines(density(iris$Sepal.Width), col="green") # note que agora estamos usando a funcao o comando add=TRUE

  par(mfrow=c(1, 1))

### Box-plot

  # Criando o box plot
  boxplot(iris$Sepal.Length)
  boxplot(iris$Sepal.Width)
  boxplot(iris$Petal.Length)
  boxplot(iris$Petal.Width)

  # Agora vamos olhar para os valores por espécie.
  boxplot(Sepal.Length ~ Species, data = iris)
  boxplot(Sepal.Width ~ Species, data = iris)
  boxplot(Petal.Length ~ Species, data = iris)
  boxplot(Petal.Width ~ Species, data = iris)

  # Identificando os outliers
  boxplot(iris$Sepal.Width)

  my_boxplot <- boxplot(iris$Sepal.Width, plot=FALSE)
  my_boxplot

  # o objeto é uma lista e os valores outliers estão guardados no elemento $out da lista
  outliers <- my_boxplot$out

  #qual a posicao dos outliers
  which(iris$Sepal.Width %in% outliers)

  # vamos usar a posicao para indexar o objeto
  iris[which(iris$Sepal.Width %in% outliers), c("Sepal.Width", "Species")]

  #  identificando outliers de maneira espécie específica
  boxplot(Sepal.Width ~ Species, data = iris)

  my_boxplot2 <- boxplot(Sepal.Width ~ Species, data=iris, plot=FALSE)
  my_boxplot2

  outliers2 <- my_boxplot2$out

  # neste caso, queremos apenas os outliers da especie setosa
  # vamos usar a posicao para indexar o objeto
  iris[iris$Sepal.Width %in% outliers2 &
         iris$Species == "setosa",
       c("Sepal.Width", "Species")]

### COmparando dados morfométricos com uma distribuição normal
  par(mfrow = c(1,3))
  qqnorm(iris$Sepal.Length[iris$Species == "setosa"],
         main = "setosa")
  qqline(iris$Sepal.Length[iris$Species == "setosa"])
  qqnorm(iris$Sepal.Length[iris$Species == "versicolor"],
         main = "versicolor")
  qqline(iris$Sepal.Length[iris$Species == "versicolor"])
  qqnorm(iris$Sepal.Length[iris$Species == "virginica"],
         main = "virginica")
  qqline(iris$Sepal.Length[iris$Species == "virginica"])

  par(mfrow=c(1,1))

### Relação entre variáveis

  install.packages("GGally")
  library("GGally")
  ggpairs(vars)


### Criando e salvando gráficos

  # lendo os dados
  comm <- read.csv("data/cestes/comm.csv")
  envir <- read.csv("data/cestes/envir.csv")

  # explore os dados com as funções head e summary
  head(comm)
  summary(comm)
  head(envir)
  summary(envir)

  # extraindo riqueza por área
  comm.pa <- comm[, -1] > 0
  rich <- apply(X = comm.pa, MARGIN = 1, FUN = sum)
  head(rich)
  summary(rich)

  # Criando o boxplot da riqueza
  boxplot(rich)

  # Mudando os valores do eixo y de verticais para horizontais
  boxplot(rich, las = 1)

## Criando uma nova tabela com a coluna de riqueza
  localidades <- cbind(envir, rich)
  head(localidades)

# Gráfico de dispersão

  #plot com abline do modelo de regressão

  # criando modelos lineares
  riqsilt <- lm(rich ~ Silt, data = localidades)
  riqclay <- lm(rich ~ Clay, data = localidades)
  riqsand <- lm(rich ~ Sand, data = localidades)

  # extraindo os coeficientes do modelo
  coef_s <- coef(riqsilt)
  coef_c <- coef(riqclay)
  coef_d <- coef(riqsand)

  # definindo os limites dos eixos
  limy <- c(min(localidades$rich),
            max(localidades$rich))
  limx <- c(min(localidades[,c("Clay", "Sand", "Silt")]),
            max(localidades[,c("Clay", "Sand", "Silt")]))

  # definindo o nome do eixo y
  laby <- "Riqueza de espécies"

  # Construindo o gráfico em si

  # define parametros graficos
  par(mfrow = c(1, 3),
      las = 1,
      bty = "l") # aqui estamos usando las e bty dentro do par para fixar para todas as janelas

  # plot da riqueza em função do teor de Silte
  plot(rich ~ Silt, data = localidades,
       col = "tomato",
       ylim = limy, xlim = limx,
       ylab = laby,
       xlab = "Teor de Silte (%)")

  # linha do previsto pelo modelo
  ## a + b*x
  abline(a = coef_s[1], b = coef_s[2],
         col = 'tomato', lwd = 2)
  mtext("A", 3, adj = 0, font = 2)

  ## plot da riqueza em função do teor de Argila
  plot(rich ~ Clay, data = localidades,
       col = "navy",
       ylim = limy, xlim = limx,
       ylab = "",
       xlab = "Teor de Argila (%)")
  mtext("B", 3, adj = 0, font = 2)

  # linha do previsto pelo modelo
  ## a + b*x
  abline(a = coef_c[1],
         b = coef_c[2],
         col = 'navy',
         lwd = 2)

  ## plot da riqueza em função do teor de Areia
  plot(rich ~ Sand, data = localidades,
       col = "dodgerblue",
       ylim = limy, xlim = limx,
       ylab = "",
       xlab = "Teor de Areia (%)")
  mtext("C", 3, adj = 0, font = 2)

  # linha do previsto pelo modelo
  ## a + b*x
  abline(a = coef_d[1],
         b = coef_d[2],
         col = 'dodgerblue',
         lwd = 2)

# Exportando o gráfico com as funções png() e dev.off()

  # a funcao png cria o arquivo, daqui pra frente você não vai mais ver o gráfico
  png("figs/figura01.png", res = 300, width = 2400, height = 1200)

  # define parametros graficos
  par(mfrow = c(1, 3),
      las = 1,
      bty = "l") # aqui estamos usando las e bty dentro do par para fixar para todas as janelas

  # plot da riqueza em função do teor de Silte
  plot(rich ~ Silt, data = localidades,
       col = "tomato",
       ylim = limy, xlim = limx,
       ylab = laby,
       xlab = "Teor de Silte (%)")

  # linha do previsto pelo modelo
  ## a + b*x
  abline(a = coef_s[1], b = coef_s[2],
         col = 'tomato', lwd = 2)
  mtext("A", 3, adj = 0, font = 2)

  ## plot da riqueza em função do teor de Argila
  plot(rich ~ Clay, data = localidades,
       col = "navy",
       ylim = limy, xlim = limx,
       ylab = "",
       xlab = "Teor de Argila (%)")
  mtext("B", 3, adj = 0, font = 2)

  # linha do previsto pelo modelo
  ## a + b*x
  abline(a = coef_c[1],
         b = coef_c[2],
         col = 'navy',
         lwd = 2)

  ## plot da riqueza em função do teor de Areia
  plot(rich ~ Sand, data = localidades,
       col = "dodgerblue",
       ylim = limy, xlim = limx,
       ylab = "",
       xlab = "Teor de Areia (%)")
  mtext("C", 3, adj = 0, font = 2)

  # linha do previsto pelo modelo
  ## a + b*x
  abline(a = coef_d[1],
         b = coef_d[2],
         col = 'dodgerblue',
         lwd = 2)

  # para finalizar o gráfico e gerar o arquivo, precisamos rodar o dev.off()
  dev.off()

  ## png
  ##   2

### Modificando detalhes (cores, fontes, adicionando valores de R², etc)

# criando vetor de cores
  cores <- c("#3B9AB2", "#EBCC2A", "#F21A00")

# criando vetor com o nome das espécies
  sp <- paste("I.", unique(iris$Species), sep = " ")
  par(mfrow = c(2, 2),
      mar = c(4, 1, 1, 1),
      bty = 'l',
      las = 1)
  boxplot(Sepal.Length ~ Species,
          data = iris,
          xlab = "",
          col = cores,
          xaxt = "n")
  axis(1, at = 1:3, labels = sp, font = 3)
  boxplot(Sepal.Width ~ Species,
          data = iris,
          xlab = "",
          col = cores,
          xaxt = "n")
  axis(1, at = 1:3, labels = sp, font = 3)
  boxplot(Petal.Length ~ Species, data = iris,  col = cores,
          xaxt = "n")
  axis(1, at = 1:3, labels = sp, font = 3)
  boxplot(Petal.Width ~ Species,
          data = iris,
          col = cores,
          xaxt = "n")
  axis(1, at = 1:3, labels = sp, font = 3)

## Gráfico de média com desvio padrão com arrows

  # fixando uma semente de numeros aleatorios para manter o mesmo resultado no sample
  set.seed(42)

  # criando um data frame com valores medios e desvio padrão de uma variável
  d2 <- data.frame(name = letters[1:5],
                   value = sample(seq(4, 15), 5),
                   sd = c(1, 0.2, 3, 2, 4))

  # Fazendo o plot dos pontos
  plot(x = 1:5, d2$value, las = 1, bty = 'l',
       ylim = c(0, 18), pch = 19, xaxt = 'n',
       xlab = "names", ylab = "value")
  # Adicionando a configuração do eixo x na mão com a função axis
  axis(1, at = 1:5, labels = d2$name)
  # Adicionando os valores de desvio padrão em torno da média com a função arrows
  arrows(x0 = 1:5,
         y0 = d2$value + d2$sd,
         y1 = d2$value - d2$sd, angle = 90, length = 0.05, code = 3)


