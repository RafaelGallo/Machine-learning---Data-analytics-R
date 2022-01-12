#Q-1
nome<-c("Marcos de Aguiar", "João Silva", "Karla Santos")
nome

salario<-c(4000, 3000, 5000)
salario

idade<-c(34, 35, 47)
idade

clientes<-cbind(nome, salario, idade)
clientes

nome2<-c("Paulo Silva", "Julia Dantas", "Marcia silva", "Daniel")
nome2

salario<-c(5.000, 6.000, 7.000, 8.000)
salario

clientes<-cbind(nome, salario, idade)
clientes

#Q-2
salario<-c(5.500,33)
salario

salario[2,2]<-5500.33
salario

#Q-3
clientes[,2]<-as.numeric(clientes[,2])*0.7
clientes

#Q-4
mean(as.numeric(clientes[,2]))

#Q-5
colnames(clientes)<-c("Nome","Salário","Idade")
colnames
