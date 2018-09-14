#
# Script para criacao do modelo e avaliacao
#

load("/data/20140917_imoveis_completo_tratado.rda")
sort(table(imoveis$bairro), decreasing = TRUE)
length(table(imoveis$bairro))

# separando o dataset de treino com o de teste (aleat?riamente, pq n sao dados categ?ricos)
set.seed(1234)
ind <- sample(2, nrow(imoveis), replace = TRUE, prob = c(0.8, 0.2))
#a função sample acima faz um sorteio entre 1 e 2 para criar indices para dividir o dataset entre
train <- imoveis[ind == 1, ]
test <- imoveis[ind == 2,]

model <- lm(preco ~ ., data=train) #traz a fun??o para aprender uma regress?o linear (quero saber pre?o em rela??o a todos os outros atributos do dataset (.))
model
summary(model) #Residuals ? o erro
plot(model) #o plot para modelos de regrassão imprime 4 gráficos. O primeiro é o mais importante


# Function that returns Root Mean Squared Error
rmse <- function(error)#declara que é uma função
{
  sqrt(mean(error^2))
} #diferença entre o que o modelo fornece e o verdadeiro (estima qual modelo erra menos de forma grosseira)

# Function that returns Mean Absolute Error
mae <- function(error)
{
  mean(abs(error))
}

test$pred <- predict(model, test) #cria uma nova coluna chamada pred na tabela test e coloca o predict
rmse(test$preco - test$pred) # é a função que criamos lá em cima para ver o quao bom o modelo tá (mais próximo de zero é melhor)
mae(test$preco - test$pred) #diferen?a absoluta entre o erro e tira uma m?dia

test$error <- abs(test$preco - test$pred) #cria coluna de erro
View(test)

#vamos descobrir se ele t? over fittando ou n?o (usando o conjunto de treinamento)
predTrain <- predict(model, train)
rmse(train$preco - predTrain)
mae(train$preco - predTrain)


