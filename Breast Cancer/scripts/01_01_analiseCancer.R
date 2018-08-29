#
# O objetivo deste script é mostrar todo o processo para
# criação de classificadores, incluindo a etapa de validação,
# através de um problema para identificação de cancer 
# benigno e maligno.
#

dataset <- read.csv("data/breast-cancer-wisconsin.csv")
dataset$X <- NULL #remove a coluna com nome x

names(dataset)

summary(dataset$diagnosis) #Resumo da coluna diagnosis - 357 Benignos e 212 Malignos
class(dataset$diagnosis)

dataset$diagnosis
dataset$id <- NULL#vamos ignorar e tirar o ID
nrow(dataset$diagnosis == "M") # ?
sum(dataset$diagnosis == "M") # Soma dos M da coluna Diagnosis do dataset
sapply(dataset, class)  # dá a classe de todas as colunas da tabela
summary(dataset)

#################################

#obs1 Atributos do dataset se distribuem em médias e desvios padrão
#obs2 o que queremos é identificar o padrão dos canceres benignos e malignos

#obs3 é muito comum no dia a dia faltarem valores, por isso vamos identificar se há algum valor NA
sum(is.na(dataset)) #is.na(dataset)

#Qual objetivo? Criar um modelo preditivo para me dizer se um Cancer é ou nao benigno

#Para isso, vamos dividir o conjunto em treinamento e teste 80% treinamento (457) e 20%(112) de teste
#Contudo, a qtd tem que ser exatamente dividida entre benignos e malignos



#                               EXERCÍCIOS

# no site do kaggle existe uma descrição de cada atributo. 
# (1) Por favor, gastar 15 minutos lendo a descrição e discutindo com os colegas.

# "diagnosis" The diagnosis of breast tissues (M = malignant, B = benign)              
# "radius_mean" mean of distances from center to points on the perimeter            
# "texture_mean" standard deviation of gray-scale values            
# "perimeter_mean" mean size of the core tumor          
# "area_mean"               
# "smoothness_mean" mean of local variation in radius lengths         
# "compactness_mean" mean of perimeter^2 / area - 1.0        
# "concavity_mean" mean of severity of concave portions of the contour          

# (2) existe algum registro com valores nulos? 
sum(is.na(dataset))

# (3) qual é o tamanho do dataset? 
nrow(dataset)

# repensar o objetivo do exercício.
# criar um modelo preditivo capaz de identificar cancer benigno e cancer maligno
# este modelo precisa ser genérico.
#
# Ler o texto "6 Your dev and test sets should come from the same distribution"
# do arquivo docs/references/Ng_MLY01.pdf

  #O texto explica que o conjunto de desenvolvimento (dev) e teste(test) devem ter a mesma distribuição de atributos. 
  #Por exemplo: se tivermos 100 linhas no dataset e 1/2 é Benigno e 1/2 maligno, precisamos garantir que os 80% de dev tenham a proporção de 1/2 pra cada e 0s 20% de test também; 1/2.
  #Isso ajuda a eliminar incertezas em possíveis futuras falhas de overfitting. Se a distribuição estiver certa, vc sabe que que não foi problema dela, e sim que seu algoritmo se adaptou demais ao dev set 

# (4) separar conjunto de treinamento e de teste usando a mesma distribuicao do 
# atributo diagnosis
## install.packages("caret")
library(caret)

# Breve e ótima explicação de por que usar o set.seed(): https://stackoverflow.com/questions/13605271/reasons-for-using-the-set-seed-function
# Resumo: Serve para "Reprodução do algoritmo" 
set.seed(1234)
#queremos particionar o dataset com a coluna diagnosis sendo 80% para dev(treinamento) e 20% para teste
trainIndex <- createDataPartition(dataset$diagnosis, p = .8,
                                  list = FALSE,
                                  times = 1) #aqui serve para criar o índice da partição
train <- dataset[trainIndex,] #Aqui Particiona
test <- dataset[-trainIndex,] #Aqui Particiona
 
summary(train$diagnosis)
summary(test$diagnosis)

sum(test$diagnosis == "B")/sum(test$diagnosis == "M")
sum(train$diagnosis == "B")/sum(train$diagnosis == "M")
#repare que ele respeita as proporcoes do diagnosis

# (5) criando o modelo com os atributos selecionados
install.packages("randomForest")
library(randomForest)

model <- randomForest(diagnosis ~ ., data=train, importance=TRUE, do.trace=100)
#eu quero aprender o valor da variável diagnosis. o ponto depois do til(~) serve para pegar todas as outras colunas. 
#o Y é a coluna diagnosis(que queremos prever) e X são todas as outras 30

model2 <- randomForest(diagnosis ~ radius_mean + texture_mean, data=train, importance=TRUE, do.trace=100)
#model2 usa somente radius_mean + texture_mean para montar o modelo
model

# (6) validando o modelo completo no conjunto de teste
predict(model, newdata = test)

testPred <- predict(model, newdata = test)
t <- table(testPred, test$diagnosis) 
#Essa tabela (t) mostra o quanto de benignos foram realmente compreendidos como benignos ou como malignos e vice versa 

(67+41)/(67+1+4+41) #Acurácia do modelo (Acertos/QTD de previsoes feitas)
5/113 #% de erros do modelo (Erros/QTD de previsoes feitas)

#Não sei que porra é essa
install.packages("e1071")
library("e1071")
confusionMatrix(t)

# discutir os conceitos de accuracy, confusion matrix, sensitivity and specificity
# usar este texto como referência: https://en.wikipedia.org/wiki/Sensitivity_and_specificity

