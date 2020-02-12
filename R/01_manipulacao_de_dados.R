##Script para manipulação de dados em bases relacionais

#carregando os pacotes necessários
library("tidyr")

#Help da função
?list.files

#Criando o vetor com os 5 arquivos
files.path <- list.files(path = "data/cestes",
                         pattern = ".csv",
                         full.names = TRUE)

#Criando os objetos do vetor
files.path
comm <- read.csv(files.path[1])
coord <- read.csv(files.path[2])
envir <- read.csv(files.path[3])
splist <- read.csv(files.path[4])
traits <- read.csv(files.path[5])

#Entendendo os objetos
  #Objeto comm
    #Visualizando os dados dentro do objeto
head(comm)

#Contando quantas observações e variáveis tem o objeto
dim(comm)

#Resumindo o objeto
summary(comm)

#Verificando de quantas espécies temos os dados
nrow(splist)

#Verificando quantas áreas amostradas temos
nrow(envir)

#Verificando quantas variáveis ambientais temos

  # todas as variáveis exceto a primeira coluna com o id
  names(envir)[-1]
  # contando quantas variáveis
  length(names(envir)[-1])

#Verificando a riqueza de cada área

  #Para isso preciso transformar a matriz (dados de abundância para dados de presença e ausência)

  #Crio o objeto com a planilha de abundância e excluo a primeira coluna que tem o ID dos sites
  comm.pa <- comm[, -1] > 0
  comm.pa

  # vamos nomear as linhas das planilhas com o id dos sites
  row.names(comm.pa) <- envir$Sites
  row.names(comm.pa)

  #Calculando a riqueza da área 1 somando a primeira linha
  sum(comm.pa[1, ])

  #Calculando a soma de todas as áreas de forma automatizada para determinar a riqueza de todas as áreas
  rich <- apply(X = comm.pa, MARGIN = 1, FUN = sum)

  #Resumo dos resultados de riqueza
  summary(rich)

### Juntando diferentes tabelas

#Chamando a chave primária da tabela de variáveis ambientais
  envir$Sites

  #Resumo da chave primária
  summary(envir$Sites)


#Transformando uma variável numérica em um fatp (categórico) que é o caso da chave primária, a qual se refe a ID das áreas

  # se checarmos a classe desse vetor, veremos que é numerica
  class(envir$Sites)

  # queremos que seja uma variável categórica. Para isso, convertemos em fator
  as.factor(envir$Sites)

  # se usarmos apenas as.factor, não fazemos a conversão, vamos então fazer uma atribuição
  envir$Sites <- as.factor(envir$Sites)

  #Fazendo o mesmo para o objeto coord
  coord$Sites <- as.factor(coord$Sites)

#Juntando as duas primeiras tabelas (coord e envir)
  envir.coord <- merge(x = envir,
                       y = coord,
                       by = "Sites")
  envir.coord

# Checando a junção. Quantas colunas deveríamos ter ao final? Quais colunas foram adicionadas?

  dim(envir)
  dim(coord)
  dim(envir.coord)
  head(envir.coord)

###Transformando uma matrix espécie vs. área em uma tabela de dados

  # vetor contendo todos os Sites
  Sites <- envir$Sites
  length(Sites)

  # vetor número de espécies
  n.sp <- nrow(splist)
  n.sp

  # criando tabela com cada especie em cada area especies em linhas
  comm.df <- tidyr::gather(comm[, -1])

  #Checando cabeçalho e dimensões do objeto
  dim(comm.df)
  head(comm.df)

#Mudando o nome das colunas de um data frame

  # nomes atuais
  colnames(comm.df)

  # modificando os nomes das colunas
  colnames(comm.df) <-  c("TaxCode", "Abundance")

  # checando os novos nomes
  colnames(comm.df)

#Adicionando a coluna de ID no novo objeto

  # primeiro criamos a sequência
  seq.site <- rep(Sites, times = n.sp)

  # checando a dimensão
  length(seq.site)

  # adicionando ao objeto comm.df
  comm.df$Sites <- seq.site

  # checando como ficou
  head(comm.df)

#Juntando todas as variáveis em uma mesma tabela
  #Juntando os nomes das espécies com comm.df
  comm.sp <- merge(comm.df, splist, by = "TaxCode")
  head(comm.sp)

  #Juntando comm.sp + traits
  names(traits)
    # renomeando o primeiro elemento
    colnames(traits)[1] <- "TaxCode"
    comm.traits <- merge(comm.sp, traits, by = "TaxCode")
    head(comm.traits)

  #Juntando comm.traits + envir.coord
    comm.total <- merge(comm.traits, envir.coord, by = "Sites")
    head(comm.total)

#Exportando planilha modificada
write.csv(x = comm.total,
              file = "data/01_data_format_combined.csv",
              row.names = FALSE)

