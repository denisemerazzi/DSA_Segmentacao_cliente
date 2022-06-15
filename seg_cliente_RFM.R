# Big Data na Prática 4 - realizado em 25/05/2022
# Segmentação de Clientes com Base em Análise RFM (Recência, Frequência e Valor Monetário)


# Configurando o diretório de trabalho

setwd("C:/Users/deeww/OneDrive/Área de Trabalho/Ciencia de dados/Curso Formação Cientista de dados/BigData_R_Azure_ML/cap06")
getwd()


# Imports
library(tidyverse)
library(dplyr)
library(ggplot2)
library(caret)
library(plotly)
library(readxl)
library(rfm)
library(stats)
library(factoextra)

# Função para carregar os dados da planilha Excel
carrega_dados <- function()
{
  setwd('C:/Users/deeww/OneDrive/Área de Trabalho/Ciencia de dados/Curso Formação Cientista de dados/BigData_R_Azure_ML/cap06')
  sheet1 <- read_excel('online_retail_II.xlsx', sheet = 'Year 2009-2010')
  sheet2 <- read_excel('online_retail_II.xlsx', sheet = 'Year 2010-2011')
  dados_combinados <- rbind(sheet1, sheet2)
  return(dados_combinados)
}

# Executa a função
dados <- carrega_dados()
dim(dados)
View(dados)

# Função para checar valores ausentes
verifica_missing <- function(x)
{
  return(colSums(is.na(x)))
  
}

# Executa a função
verifica_missing(dados)

# Vamos apenas excluir os registros com valores ausentes

# Função para limpar e pré-processar os dados
preprocessa_dados <- function(data1)
{
  # Criando uma coluna chamada TotalPrice (quantidade de itens x seu preço) - "engenharia de atributos"
  data1$TotalPrice <- data1$Quantity * data1$Price
  
  # Remove registros com valores ausentes
  data1 <- na.omit(data1)
  
  # Removemos as linhas da coluna Invoice que contém a letra C (o que significa que este pedido foi cancelado)
  data1 <- data1[!grepl("C",data1$Invoice),]
  
  return(data1)
  
}

# Executa a função
dataset <- preprocessa_dados(dados)
View(dataset)
dim(dataset)

# Verificando a distribuição da variável Total Price
ggplot(dataset,
       aes(x = TotalPrice)) +
  geom_density(fill = "#69b3a2", color = "#e9ecef", alpha = 3.5) +
  labs(title = 'Distribuição da Variável TotalPrice')

# Número de clientes (Quando tem espaço no nome da coluna... obrigatoriamente tem que usar aspas simples)
length(dataset$`Customer ID`)  ### total de transações: 805620
length(unique(dataset$`Customer ID`))  ## clientes unicos: 5801

# Total monetário gasto por cliente (faz um groupy buscando quanto cada cliente gastou)
total_gasto <- dataset %>%
  group_by(`Customer ID`) %>%
  summarise(Sum = sum(TotalPrice))

View(total_gasto)

# Criando uma data customizada (Natal de 2011) ´Engenharia de atributos
# Qual foi a compra mais recente do cliente até o natal de 2011?
View(dataset)
max(dataset$InvoiceDate)
date1 = as.Date.character("25/12/2011","%d/%m/%Y")

# Função para converter as datas do formato POISxt para o formato Date
# É necessário converter/ ajustar a data
converte_data <- function(x)
{
  options(digits.secs = 3)
  return(as.Date(as.POSIXct(x$InvoiceDate, 'GMT')))
}

# Executa a função
dataset$InvoiceDate <- converte_data(dataset)
View(dataset)

# Função para calcular Recência, Frequência e Valor Monetário - verifica-se os outliers
calcula_rfm <- function(x){
  z <- x %>% group_by(`Customer ID`) %>% 
     summarise(Recency = as.numeric(date1 - max(InvoiceDate)), 
               Frequency = n(), 
               Monetary = sum(TotalPrice),
               primeira_compra = min(InvoiceDate))
  
  # Removendo transações com valores acima do 3º Quartil e abaixo do Quartil 1 (removendo outliers)
  Q1 <- quantile(z$Monetary, .25)
  Q3 <- quantile(z$Monetary, .75)
  IQR <- IQR(z$Monetary)
  z <- subset(z, z$Monetary >= (Q1 - 1.5*IQR) & z$Monetary <= (Q3 + 1.5*IQR))
  return(z)
}

# Executa a função
valores_rfm <- calcula_rfm(dataset)
View(valores_rfm)

# Machine Learning - Clusterização Kmeans (ML não supervisionada)
# Compara a distância entre os dados, baseado na Distância Euclidina

# Set seed (experimento aleatório só que começa sempre do mesmo lugar - para poder ter os resultados quando outra pessoa for reproduzir)
set.seed(1029)

# Função para a segmentação de clientes com base nos valores RFM
segmenta_cliente <- function(rfm)
{
  # Cria uma lista
  resultados <- list()
  
  # Obtém os valores RFM
  dados_rfm <- select(rfm, c('Recency','Frequency','Monetary'))
  
  # Cria o modelo (cria 5 grupos) - modelo treinado
  modelo_kmeans <- kmeans(dados_rfm, center = 5, iter.max = 50)
  
  # Plot do modelo - resultado
  resultados$plot <- fviz_cluster(modelo_kmeans, 
                                  data = dados_rfm, 
                                  geom = c('point'), 
                                  ellipse.type = 'euclid')
  
  # Organiza os dados - JUntando o resultado da segmentação dos cluster e associando ao ID...
  # Para saber qual cliente participa de qual grupo ;-)
  
  dados_rfm$`Customer ID` <- rfm$`Customer ID`
  dados_rfm$clusters <- modelo_kmeans$cluster
  resultados$data <- dados_rfm
  
  return(resultados)
}

# Executa a função
grafico <- segmenta_cliente(valores_rfm)[1]  # 1 é o plot
grafico

tabela_rfm <- segmenta_cliente(valores_rfm)[2]   # 2 são os dados
View(as.data.frame(tabela_rfm))






