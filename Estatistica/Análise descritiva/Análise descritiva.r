########################### Análise Descritiva ###########################


# Trabalho de Análise Descritiva de um Conjunto de Dados
# Análise descritiva básica de um conjunto de dados retirados da Pesquisa Nacional 
# por Amostra de Domicílios - 2015 do IBGE.

# DATASET DO PROJETO
# Pesquisa Nacional por Amostra de Domicílios - 2015
# A Pesquisa Nacional por Amostra de Domicílios - PNAD investiga anualmente, de forma permanente, características gerais da população, de educação, trabalho, rendimento e 
# habitação e outras, com periodicidade variável, de acordo com as necessidades de informação para o país, 
# como as características sobre migração, fecundidade, nupcialidade, saúde, segurança alimentar, entre outros temas. O levantamento dessas estatísticas constitui, ao longo dos 49 anos de realização da pesquisa, um importante instrumento para formulação, validação e avaliação de políticas orientadas para o desenvolvimento 
# socioeconômico e a melhoria das condições de vida no Brasil.

### Fonte dos Dados
## https://ww2.ibge.gov.br/home/estatistica/populacao/trabalhoerendimento/pnad2015/microdados.shtm


# Variáveis utilizadas

# Renda
# Rendimento mensal do trabalho principal para pessoas de 10 anos ou mais de idade.

# Idade
# Idade do morador na data de referência em anos.

# Altura (elaboração própria)
# Altura do morador em metros.

# UF
# Sexo
# Anos de Estudo
# Cor

# Observação
# Os seguintes tratamentos foram realizados nos dados originais:

# Foram eliminados os registros onde a Renda era inválida (999 999 999 999);
# Foram eliminados os registros onde a Renda era missing;
# Foram considerados somente os registros das Pessoas de Referência de cada domicílio (responsável pelo domicílio).


library(dplyr)
library(ggplot2)

options(repor.plot.width = 9, repr.plot.height = 6)
formatos <- theme(
  plot.title = element_text(size = 14, hjust = 0.5),
  axis.title.y = element_text(size = 12, vjust = +0.2),
  axis.title.x = element_text(size = 12, vjust = -0.2),
  axis.text.y = element_text(size = 10),
  axis.text.x = element_text(size = 10)
)

df <- read.csv("dados.csv")

head(df)


# Para avaliar o comportamento da variável RENDA vamos construir uma tabela de frequências considerando as seguintes classes em salários mínimos (SM)
# Descreva os pontos mais relevantes que você observa na tabela e no gráfico.
# Classes de renda:
# A ► Acima de 25 SM
# B ► De 15 a 25 SM
# C ► De 5 a 15 SM
# D ► De 2 a 5 SM
# E ► Até 2 SM
# Para construir as classes de renda considere que o salário mínimo na época da pesquisa era de R$ 788,00.
# Siga os passos abaixo:

# 1º Definir os intevalos das classes em reais (R$)
real <- c(
  min(df $ Renda),
  2 * 788,
  5 * 788,
  15 * 788,
  25 * 788,
  max(df $ Renda)
)
real

# 2º Definir os labels das classes
lab <- c("E", "D", "C", "B", "A")
lab

# 3º Construir a coluna de frequências
freq <- table(
  cut(
    x = df$Renda, 
    breaks = real, 
    lab = lab, 
    include.lowest = TRUE
  )
)
freq

# 4º Construir a coluna de percentuais
per <- prop.table(freq) * 100
per

# Juntar as colunas de frequência e percentuais e ordenar as linhas de acordo com os labels das classes
dist_fre <- cbind("Frequencia" = freq, "Porcentagem (%)" = per)
dist_fre

dist_fre[
  order(row.names(dist_fre)),
  ]

# Construa um gráfico de barras para visualizar as informações da tabela de frequências acima
bar <- data.frame(dist_fre)
bar

ggplot(bar, aes(x = row.names(bar), y = bar$Frequencia)) +
  geom_bar(stat = "identity") +
  ylab("Frequência") +
  xlab("Classes de Renda") +
  ggtitle("Gráfico Classes de Renda") +
  formatos

# Crie um histograma para as variáveis QUANTITATIVAS de nosso dataset
# Descreva os pontos mais relevantes que você observa nos gráficos (assimetrias e seus tipos, possíveis causas para determinados comportamentos etc.)

ggplot(df, aes(x = Idade)) +
  geom_histogram(bins = 50) +
  ylab("Frequência") +
  xlab("Idades") +
  ggtitle("Histograma das Idades") +
  formatos

ggplot(df, aes(x = Altura)) +
  geom_histogram(bins = 30) + 
  ylab("Frequência") +
  xlab("Altura") +
  ggtitle("Histograma das Alturas") +
  formatos

ggplot(df, aes(x = Renda)) +
  geom_histogram(bins = 100) +
  ylab("Frequência") +
  xlab("R$") +
  ggtitle("Histograma das Rendas") +
  formatos

ggplot(df[df $ Renda < 20000, ], aes(x = Renda)) +
  geom_histogram(bins = 30) +
  ylab("Frequência") +
  xlab("R$") +
  ggtitle("Histograma das Rendas - Pessoas com renda até R$ 20.000,00") +
  formatos

# Construa uma tabela de frequências e uma com os percentuais cruzando das variáveis SEXO e COR
# Avalie o resultado da tabela e escreva suas principais conclusões
# Utilize os vetores abaixo para renomear as linha e colunas das tabelas de frequências e dos gráficos em nosso projeto

sexo = c(
  'Masculino', 
  'Feminino'
)
cor = c(
  'Indígena', 
  'Branca', 
  'Preta', 
  'Amarela', 
  'Parda'
)
anos_de_estudo = c(
  'Sem instrução e menos de 1 ano', 
  '1 ano', 
  '2 anos', 
  '3 anos', 
  '4 anos', 
  '5 anos', 
  '6 anos', 
  '7 anos', 
  '8 anos', 
  '9 anos', 
  '10 anos', 
  '11 anos', 
  '12 anos', 
  '13 anos', 
  '14 anos', 
  '15 anos ou mais', 
  'Não determinados'
)

# Sexo
df$Cat.Sexo <- factor(df$Sexo)
levels(df$Cat.Sexo) <- sexo

# Cor
df$Cat.Cor <- factor(df$Cor)
levels(df$Cat.Cor) <- cor

# Anos de Estudo
df$Cat.Anos.de.Estudo <- factor(df$Anos.de.Estudo, order = TRUE)
levels(df$Cat.Anos.de.Estudo) <- anos_de_estudo

head(df)

freq <- table(df$Cat.Sexo, df$Cat.Cor)
freq <- cbind(freq)
freq

per <- cbind(prop.table(freq) * 100)
per

# Realize, para a variável RENDA, uma análise descritiva 
# com as ferramentas que aprendemos em nosso treinamento.

# Obtenha a média aritimética
mean(df$Renda)

# Obtenha a mediana
median(df$Renda)

# Obtenha a moda
Moda <- function(x) {
  frequencias <- table(x) 
  return(names(frequencias)[frequencias == max(frequencias)])
}

as.numeric(Moda(df$Renda))

# Obtenha a variância
var(df $ Renda)

# Obtenha o desvio-padrão
sd(df $ Renda)


#Obtenha a média, mediana e valor máximo da variável RENDA segundo SEXO e COR
# Destaque os pontos mais importante que você observa nas tabulações
### Utilize tapply com as funções mean, median e max ###

med <- tapply(df$Renda, list(df$Cat.Sexo, df$Cat.Cor), mean)
med

med <- tapply(df$Renda, list(df$Cat.Sexo, df$Cat.Cor), median)
med

max <- tapply(df$Renda, list(df$Cat.Sexo, df$Cat.Cor), max)
max

# Obtenha as medidas de dispersão da variável RENDA segundo SEXO e COR
# Destaque os pontos mais importante que você observa nas tabulações

### Utilize tapply com as funções var e sd ###

var <- tapply(df$Renda, list(df$Cat.Sexo, df$Cat.Cor), var)
var

des_p <- tapply(df$Renda, list(df$Cat.Sexo, df$Cat.Cor), sd)
des_p

# Construa um box plot da variável RENDA segundo SEXO e COR
# É possível verificar algum comportamento diferenciado no rendimento entre os grupos de pessoas analisados? Avalie o gráfico e destaque os pontos mais importantes;

# 1º - Utilize somente as informações de pessoas com renda abaixo de R$ 10.000
# 2º - Para incluir uma terceira variável na construção de um boxplot utilize o parâmetro fill da seguinte maneira: aes(x = Cor, y = Renda, fill = Sexo).

ggplot(data = df[df $ Renda < 10000, ], aes(x = Cat.Cor, y = Renda, fill = Cat.Sexo)) +
  geom_boxplot(size = 0.2) +
  coord_flip() +
  ylab("R$") +
  xlab("Cor") +
  guides(fill = guide_legend(title = "Sexo")) +
  ggtitle("Box-plot da RENDA por SEXO e COR") +
  formatos

# Qual percentual de pessoas de nosso dataset ganham um salário mínimo (R$ 788,00) ou menos?
length(df$Renda[df$Renda <= 788]) / length(df$Renda) * 100

# Qual o valor máximo ganho por 99% das pessoas de nosso dataset?
quantile(df$Renda, .99)


# Obtenha a média, mediana, valor máximo e desvio-padrão da variável RENDA segundo ANOS DE ESTUDO e SEXO

### Destaque os pontos mais importante que você observa nas tabulações ###
medias <- tapply(df$Renda, list(df$Cat.Anos.de.Estudo, df$Cat.Sexo), mean)
medias

medianas <- tapply(df$Renda, list(df$Cat.Anos.de.Estudo, df$Cat.Sexo), median)
medianas

maximos <- tapply(df$Renda, list(df$Cat.Anos.de.Estudo, df$Cat.Sexo), max)
maximos

desvio_padrao <- tapply(df$Renda, list(df$Cat.Anos.de.Estudo, df$Cat.Sexo), sd)
desvio_padrao

# Construa um box plot da variável RENDA segundo ANOS DE ESTUDO e SEXO
# É possível verificar algum comportamento diferenciado no rendimento entre os grupos de pessoas analisados? Avalie o gráfico e destaque os pontos mais importantes.

# 1º - Utilize somente as informações de pessoas com renda abaixo de R$ 10.000
# 2º - Para incluir uma terceira variável na construção de um boxplot utilize o parâmetro fill da seguinte maneira: aes(x = Anos.de.Estudo, y = Renda, fill = Sexo).

ggplot(data = df[df$Renda < 10000, ], aes(x = Cat.Anos.de.Estudo, y = Renda, fill = Cat.Sexo)) + 
  geom_boxplot(size = 0.2) + 
  coord_flip() +
  ylab("R$") + 
  xlab("Anos de Estudo") + 
  guides(fill = guide_legend(title = 'Sexo')) +
  ggtitle('Box-plot da RENDA por SEXO e ANOS DE ESTUDO') +
  formatos

