#
# O objetivo deste script é mostrar todo o processo para
# criação de classificadores, incluindo a etapa de validação,
# através de um problema para identificação de cancer 
# benigno e maligno.
#

dataset <- read.csv("data/breast-cancer-wisconsin.csv")
dataset$X <- NULL #remove a coluna com nome x

names(dataset)

summary(dataset$diagnosis) #Resumo da coluna diagnosis
class(dataset$diagnosis)
dataset$id <- NULL#vamos ignorar e tirar o ID
nrow(dataset$diagnosis == "M") # ?
sapply(dataset, class)  # dá a classe de todas as colunas da tabela
summary(dataset)

#################################

#obs1 Atributos do dataset se distribuem em médias e desvios padrão
#obs2 o que queremos é identificar o padrão dos canceres benignos e malignos
#obs3 é muito comum no dia a dia faltarem valores, por isso vamos identificar se há algum valor NA
sum(is.na(dataset))
#Qual objetivo? Criar um modelo preditivo para me dizer se um Cancer é ou nao benigno
#Para isso, vamos dividir o conjunto em treinamento e teste 80% treinamento (457) e 20%(112) de teste
#Contudo, a qtd tem que ser exatamente dividida entre benignos e malignos

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

# (4) separar conjunto de treinamento e de teste usando a mesma distribuicao do 
# atributo diagnosis
install.packages("caret")
library(caret)
set.seed(1234)

trainIndex <- createDataPartition(dataset$diagnosis, p = .8,
                                  list = FALSE,
                                  times = 1)
#queremos particionar o dataset com a coluna diagnosis 
train <- dataset[trainIndex,]
test <- dataset[-trainIndex,]
 
summary(train$diagnosis)
summary(test$diagnosis)
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

(67+41)/(67+1+4+41) #Acurácia do modelo
5/113
install.packages("e1071")
confusionMatrix(t)

# discutir os conceitos de accuracy, confusion matrix, sensitivity and specificity
# usar este texto como referência: https://en.wikipedia.org/wiki/Sensitivity_and_specificity

