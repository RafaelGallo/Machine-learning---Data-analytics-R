library(dplyr)
library(cluster)
library(fpc)
library(reshape2)
library(ggplot2)

filmes <- read.csv('movies.csv',stringsAsFactors = F)
head(filmes)

filmes_transf <- read.csv2('movies_transf.csv')
head(filmes_transf)

filmes_transf <- filmes_transf %>% 
  select(-movieId, -titulo)

dados_normalizados <- scale(filmes_transf)
dados_normalizados

dados_normalizados <- data.frame(dados_normalizados)
dados_normalizados

#K-Means
set.seed(1987)

resultado_cluster <- kmeans(dados_normalizados, centers = 3)
resultado_cluster

#Cluster
resultado_cluster$cluster

#Centroides
resultado_cluster$centers
head(resultado_cluster)
head(resultado_cluster$centers)

#Cluster
resultado_cluster$withinss
resultado_cluster$size

#Grafico
clusplot(x = filmes_transf, resultado_cluster$cluster, color = TRUE, shade =  TRUE)

plotcluster(x = dados_normalizados, resultado_cluster$cluster,ignorenum = T)

centros <- resultado_cluster$centers
centros

centros_2 <- melt(centros)
centros_2

colnames(centros_2) <- c('cluster', "genero", "centro")
centros <- centros_2$cluster <- as.factor(centros_2$cluster)
centros

ggplot(data = centros_2 ) + 
  geom_bar(aes(x = genero,y = centro,fill = cluster),stat = 'identity') + 
  facet_grid(cluster ~ .)

soma_quadrados <- 0
set.seed(1987)

range_k <- c(1:25)
range_k

for (i in range_k){
  cluster <- kmeans(dados_normalizados, centers = i, nstart = 25)
  soma_quadrados[i] <- sum(cluster$withinss)
}

soma_quadrados

plot(range_k, soma_quadrados, type = 'b',
     xlab = "Numero de clusters",
     ylab = "Soma dos Quadrados")
axis(side = 1, at = range_k, labels = range_k)
abline(v = 5, col = "red")

media_silhouete <- c(0)
media_silhouete

range_k <- c(2:20)
range_k

set.seed(1987)

#Modelo do K-Means
for (i in range_k){
  print(i)
  clusters  <- kmeans(dados_normalizados,  centers = i) 
  Silhouete <- silhouette(clusters$cluster, dist(dados_normalizados)) 
  media_silhouete[i]   <-  mean(Silhouete[,3]) 
}

media_silhouete 

plot(media_silhouete ,
     type = "b", 
     xlab = "Número de Cluster(k)",
     ylab = "Média Silhouettes")
axis(side=1,at=range_k, labels=range_k)
abline(v=12,col="red")

set.seed(1987)
resultado_cluster <- kmeans(dados_normalizados, centers = 12)
resultado_cluster

centros <- resultado_cluster$centers
centros

centros_2 <- melt(centros)
centros_2

colnames(centros_2) <- c('cluster','genero','centro')
colnames(centros_2)

centros_2$cluster   <- as.factor(centros_2$cluster)
centros_2$cluster

ggplot(data = centros_2 ) + 
  geom_bar(aes(x = genero,y = centro,fill = cluster),stat = 'identity') + 
  facet_grid(cluster ~ .)

filmes$cluster <- resultado_cluster$cluster
filmes$cluster

##Fazendo recomenda��o 
agrupamento <- filmes[filmes$title == 'Toy Story (1995)','cluster']
agrupamento

##Selecinando 10 filmes dentro do cluster
filmes[filmes$cluster == agrupamento, 'title'] %>% 
  sample(20)

matriz_dist <- dist(centros)
matriz_dist

clust_h <- hclust(matriz_dist)
clust_h

plot(clust_h, hang = -1)

#############Teste de recomenda��o########
set.seed(1987)

resultado_cluster <- kmeans(dados_normalizados,centers = 20)
resultado_cluster

centros <- resultado_cluster$centers
centros

set.seed(1987) 

matriz_dist <- dist(centros)
matriz_dist

clust_h <- hclust(matriz_dist) 
clust_h

plot(clust_h)
plot(clust_h,hang = -1)
