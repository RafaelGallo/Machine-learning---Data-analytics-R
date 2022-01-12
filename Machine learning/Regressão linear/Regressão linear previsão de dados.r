library(readxl)
library(readxl)
dataset <- read_excel(NULL)
View(dataset)

dados = dados <- read_excel("dados.xlsx")
head(dados)

head(dados)
dados

tail(dados)

hist(dados$preco)
hist(dados$area)
hist(dados$tempo)
plot(dados$area)

#Regressão linear
plot(dados$area,dados$preco)
plot(dados$area,dados$preco,main = "Diagrama de Dispersão", xlab = "Area", ylab = "Preço das casas")

#Correlação de dados
cor(dados$area, dados$preco)
cor.test(dados$area,dados$preco)

#Dados do tempo
plot(dados$tempo,dados$preco)
cor.test(dados$tempo, dados$preco)

#Aula - 2 Boxplot
boxplot(dados$preco)

#dados estatistico
summary(dados$preco)

library(car)
boxplot(dados$preco)
dados$preco[79]
which(dados$preco > quantile(dados$preco, 0.75))

#Aula 03 - Equação da reta
mod1 = lm(preco ~ area, data = dados)
mod1

View(mod1)
mod1

preco_70 = 502347 + 7851*70
preco_70

#Coeficiente
mod1$coefficients[[1]] + mod1$coefficients[[2]]*70
mod1$coefficients

#Grafico da regressão linear 2
plot(dados$area,dados$preco, main = "Diagrama e reta")
abline(mod1, col = 'blue')
summary(mod1)

#Comprovação do Modelo
names(summary(mod1))

#Estima as casas 
summary(mod1)$r.squared

#Aula 03 Validação do modelo
plot(mod1$residuals)
hist(mod1$residuals)
plot(mod1$residuals)
identify(mod1$residuals,n=2)

dados_82 = dados[44]
dados_82

dados_82_44 = dados[c(82, 44),]
dados_82_44

library(lmtest)

dwtest(mod1)

plot(mod1$fitted.values, mod1$residuals)

library(lmtest)
bptest(mod1)

plot(mod1,2)

shapiro.test(mod1$residuals)

#Aula 04 - Previsão

dados_novos = data.frame(area = c(60,70))
dados_novos

#Previssão dos dados 
predict(mod1, newdata = dados_novos)

#Previsão estimativa pontual, e limite superior e limite inferior
#Intervalo pontual da casa 60 M2, 70M2. Valores abaixo

predict(mod1, newdata = dados_novos, interval = "prediction")
predict(mod1, newdata = dados_novos, interval = "confidence")