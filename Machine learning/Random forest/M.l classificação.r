library(DAAG)
library(ggplot2)
library(lattice)

data("dengue")
names(dengue)
summary(dengue)

mb<-glm(NoYes~humid, data=dengue, family=binomial)
mb

summary(mb)

# p<0.05 a variavel é significativo

ggplot(dengue, aes(x = humid, y=NoYes)) +
  geom_point() +
  geom_smooth(method="glm", method.args = list(family = "binomial"))

hn <- data.frame(humid=23)
hn

#Previsão 
previsao1 = predict(mb, hn, type = "response")
previsao1 # Foco de dengue na região

#Teste
mx<-glm(NoYes~temp, data=dengue, family=binomial)
mx

n1 = novovalortemp<-data.frame(temp = 27)
n1

#O valor foi de 0.82, ou seja, 82% de ter um foco nessa área, baseado apenas na temperatura ambiente.
predict(mx, novovalortemp, type="response")

#Modelo
mb2 <- glm(NoYes~humid+temp, data=dengue, family = binomial)
mb2

summary(mb2)

hn2 <-data.frame(humid = 23, temp = 20)
hn2

previsao2 = predict(mb2, hn2, type = "response")
previsao2 # Foco de dengue na região

# Decision boundaries
xyplot(temp~humid, data=dengue, groups = NoYes)


#Machine learning modelo
library(caret) #modelos de ml
set.seed(123) # controlar a aleatoriedade

dengue2 = dengue2 <- na.omit(dengue) #Remover células vazias
dengue2

dataindex <- createDataPartition(dengue2$NoYes, p= .7, list=FALSE)
dataindex

#Treino e teste modelo
denguetreino <- dengue2[dataindex,] #Separando para treino
denguetreino

dengueteste <- dengue2[-dataindex,] #Separando para teste
dengueteste

#Classificador dummy
m1 = maiscomum <- sum(denguetreino$NoYes==1)/(dim(denguetreino)[1])
m1

if(maiscomum >= 0.5){
  print("classeMaisComum <- 1")
  classeMaisComum <- 1
}else{
  print("ClasseMaisComum <- 0")
  classeMaisComum <- 0
}

taxadeacerto <- sum(dengueteste$NoYes == classeMaisComum)/(dim(dengueteste)[1])
taxadeacerto

#A??rvore de decisa~o
library(rattle) # Pacote para árvore de decisão

head(as.factor(denguetreino$NoYes))

#Modelo M.L   
modelo <-train(as.factor(NoYes)~humid, data=denguetreino, method = "rpart")
modelo

#####Modelos de classificação#####
modeloML1<-train(as.factor(NoYes)~humid+temp+trees, data=denguetreino, method="glm")
modeloML1

modeloML2<-train(as.factor(NoYes)~humid+temp+trees, data=denguetreino, method="ranger")
modeloML2

modeloML3<-train(as.factor(NoYes)~humid+temp+trees, data=denguetreino, method="avNNet")
modeloML3

#Grafico
fancyRpartPlot(modelo$finalModel)

#Matriz de confusao
pGLM <- predict(modeloML1, dengueteste)
pRANGER <- predict(modeloML2, dengueteste)
pBAYES <- predict(modeloML3, dengueteste)

confusionMatrix(factor(pGLM), factor(dengueteste$NoYes))
confusionMatrix(factor(pRANGER), factor(dengueteste$NoYes))
confusionMatrix(factor(pBAYES), factor(dengueteste$NoYes))

#Curvas ROC e AUC
library(caTools)
library(pROC) # carregando o pacote para as curvas

plot.roc(dengueteste$NoYes, as.numeric(pRANGER), print.auc=TRUE)
plot.roc(dengueteste$NoYes, as.numeric(pGLM), print.auc=TRUE)
plot.roc(dengueteste$NoYes, as.numeric(pRANGER), print.auc=TRUE)

modeloML2<-train(as.factor(NoYes)~humid+temp+trees, data=denguetreino, method="ranger", importance="impurity") # adicionando o argumento "importance"

plot(varImp(modeloML2))

