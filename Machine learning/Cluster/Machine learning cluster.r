install.packages("dplyr")

filmes <- read.csv('movies.csv',stringsAsFactors = F)
View(filmes)

filmes_transf <- read.csv2('movies_transf.csv')
View(filmes_transf)

library(dplyr)
dados_normalizados <- scale(filmes_transf)

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
View(resultado_cluster)
View(resultado_cluster$centers)

#Cluster
resultado_cluster$withinss

resultado_cluster$size

install.packages("reshape")

library(cluster)

#Grafico
clusplot(x = filmes_transf, resultado_cluster$cluster, color = TRUE, shade =  TRUE)


library(fpc)
plotcluster(x = dados_normalizados, resultado_cluster$cluster,ignorenum = T)

centros <- resultado_cluster$centers
centros

library(reshape2)
centros_2 <- melt(centros)
centros_2

colnames(centros_2) <- c('cluster', "genero", "centro")
centros <- centros_2$cluster <- as.factor(centros_2$cluster)
centros

library(ggplot2)
ggplot(data = centros_2 ) + 
  geom_bar(aes(x = genero,y = centro,fill = cluster),stat = 'identity') + 
  facet_grid(cluster ~ .)

#Te??cnica Elbow
range_k <- c(1:25)
range_k

soma_quadrados <- 0
set.seed(1987)

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
range_k         <- c(2:20)
set.seed(1987)

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

##Fazendo recomendação a partir de um filme
agrupamento <- filmes[filmes$title == 'Toy Story (1995)','cluster']
agrupamento

##Selecinando 10 filmes dentro do cluster
filmes[filmes$cluster == agrupamento, 'title'] %>% 
  sample(10)

matriz_dist <- dist(centros)
matriz_dist

clust_h <- hclust(matriz_dist)
clust_h

plot(clust_h, hang = -1)
