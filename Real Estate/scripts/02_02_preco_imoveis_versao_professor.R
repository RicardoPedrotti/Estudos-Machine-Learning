#
# Script para criacao do modelo e avaliacao
#

load("../data/20140917_imoveis_completo_tratado.rda")
sort(table(imoveis$bairro), decreasing = TRUE)
length(table(imoveis$bairro))

# separando o dataset de treino com o de teste (aleatóriamente, pq n sao dados categóricos)
set.seed(1234)
ind <- sample(2, nrow(imoveis), replace = TRUE, prob = c(0.8, 0.2))
train <- imoveis[ind == 1, ]
test <- imoveis[ind == 2,]

model <- lm(preco ~ ., data=train) #traz a função para aprender uma regressão linear (quero saber preço em relação a todos os outros atributos do dataset (.))
summary(model) #Residuals é o erro
plot(model)


# Function that returns Root Mean Squared Error
rmse <- function(error)
{
  sqrt(mean(error^2))
}

# Function that returns Mean Absolute Error
mae <- function(error)
{
  mean(abs(error))
}

test$pred <- predict(model, test)
rmse(test$preco - test$pred) #
mae(test$preco - test$pred) #diferença absoluta entre o erro e tira uma média

test$error <- abs(test$preco - test$pred) #cria coluna de erro
View(test)

#vamos descobrir se ele tá over fittando ou não (usando o conjunto de treinamento)
predTrain <- predict(model, train)
rmse(train$preco - predTrain)
mae(train$preco - predTrain)
