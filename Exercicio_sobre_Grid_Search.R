#
# Trabalhando com um dataset com alta dimensionalidade,
# de separação não linear em um problema de classificacao
#

X <- read.csv("data/madelon_train.data", sep=" ")
head(X)
X$X <- NULL
Y <- read.csv("data/madelon_train.labels", sep=" ")
head(Y)
names(Y) <- c("Y")

dataset <- cbind(Y, X)
head(dataset)
class(dataset$Y)
dataset$Y <- as.factor(dataset$Y)
class(dataset$Y)

table(dataset$Y)

library(caret)
set.seed(1234)
trainIndex <- createDataPartition(dataset$Y, p = .8,
                                  list = FALSE,
                                  times = 1)
train <- dataset[trainIndex,]
test <- dataset[-trainIndex,]
train <- train[sample(nrow(train)),]
test <- test[sample(nrow(test)),]

#
# A acurácia do modelo desenvolvido usando ctree eh 0.73 no conjunto de teste
# A acurácia do modelo usando randomForest eh 0.72 no conjunto de teste
#
# Serah que nao eh possivel otimizar o modelo randomForest 
# ajustando os parametros do mesmo?
#



# Exercício 1:
# https://en.wikipedia.org/wiki/Hyperparameter_optimization
# focar em GridSearch. Primeiro usando os parametros mtry e ntree
# encontre o OOB com menor erro. Utilize este modelo para comparar com o conjunto de teste
# Você precisa encontrar um modelo que tenha no mínimo 0.80 de acurácia no conjunto de teste
# Você deve entregar uma explicação sobre o que é GridSearch e os valores dos parâmetros encontrados
# no caso do melhor modelo.

# Gridsearch é a hyper parametrização do randomforest, ou seja, estipula uma série de parametros para rodar o algoritmo de RandomForest a fim de encontrar uma alta acurácia/diminuir o erro padrão 

library(randomForest)
library(e1071)
help(randomForest)
model <- randomForest(Y ~., ntree=250, nodesize=1, mtry=300, maxnodes=1000, data=train)

predict <- predict(model, newdata=test)
table <- table(predict, test$Y)
confusionMatrix(table)

# Exercício 2: 
# E se além de ajustar os parâmetros de mtry e ntree. Ajustar também o nodesize e maxnodes? 
#
# No entanto, antes de ajustar, responda as seguintes perguntas:

# 1) O que significa o parametro nodesize?
# Significa o número mínimo de nodos que um nodo específico poderá gerar em uma árvore de decisão

# 2) O valor default deste parâmetro é 1. Isto significa o que?
# Significa que a árvore de decisão só terminará quando todos os nodos forem únicos, ou seja, não puderem gerar mais de um nodo abaixo dele.

# 3) Qual será a consequência esperada ao aumentar este número?
# Uma árvore de decisão com menos nodos, mais ágil, porém, com menor acurácia

# 4) O que significa o parametro maxnodes?
# maxnodes define o número total de nodos dentro de uma árvore

# 5) Qual eh o range de valores que devemos configurar para o GridSearch?
# O range é de 0 até o maior número "suportável" de erro definido pelo escopo. 

# 6) Encontrou um modelo ainda melhor? Quais são os valores dos parâmetros e qual a acurácia? 
# ntree=250, nodesize=1, mtry=300 , maxnodes=1000
# acurácia de 83%

# 7) Na sua opinião, qual é o principal problema do GridSearch? 
# Velocidade de execução dependendo da range de valores e tentativa para encontrar as melhores possibilidades

# A ideia do Grid Search é 
# ntree: número de árvores que o modelo vai criar (ele sempre olha 2/3 do dataset)
# mtry: altera o valor de atributos que ele vai olhar (normalmente ele olha a raiz de p -- raiz da qtd de atributos)
# nodesize: quanto maior o número pior fica. 1 significa que ele vai até o final da árvore. 10 deixa 10 exemplos de fora 

