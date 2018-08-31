#Processo CRISP-DM (Ou KDD) - procurar uma imagem na internet
#um processo que sempre inicia com uma atividade de entender o problema, logo após entender os dados, modelagem de dados, avaliação e deploy

load("data/20140917_imoveis_completo.rda")
#Business Understanding: Prever o preço de um imóvel
#Data Understanding: todos os dados parecem interessante

sum(is.na(imoveis$bairro))
sum(is.na(imoveis$preco))
sum(is.na(imoveis$area))
sum(is.na(imoveis$suites))
sum(imoveis$suites == 0)

imoveis_analise[is.na(imoveis_analise)] <- 0

class(imoveis)
summary(imoveis)

imoveis_analise <- imoveis
imoveis_analise$descricao <- NULL

#help(ifelse)

imoveis_analise <- imoveis_analise[imoveis_analise$preco != 0, ] #elimina imóveis com preço = 0

summary(imoveis_analise)
summary(imoveis_analise$area)
summary(imoveis_analise$suites)

## Tratando o Dataset
nrow(imoveis_analise[which(imoveis_analise$preco > 15000000), ]) #quantidade de linhas acima do valor
imoveis_analise <- imoveis_analise[imoveis_analise$preco < 15000000, ] #Remove 
nrow(imoveis_analise[which(imoveis_analise$banheiros > 90), ])
imoveis_analise <- imoveis_analise[imoveis_analise$preco > 0, ]
imoveis_analise <- imoveis_analise[imoveis_analise$banheiros < 90, ]
imoveis_analise <- imoveis_analise[imoveis_analise$area > 20, ]
imoveis_analise <- imoveis_analise[imoveis_analise$vagas < 10, ]
imoveis_analise <- imoveis_analise[imoveis_analise$suites < 7, ] #
imoveis_analise <- imoveis_analise[imoveis_analise$banheiros < 7, ]
imoveis_analise <- imoveis_analise[imoveis_analise$dormitorios < 7, ]
imoveis_analise <- imoveis_analise[imoveis_analise$dormitorios > 0, ]
imoveis_analise <- imoveis_analise[imoveis_analise$banheiros >= 1, ]
#é ruim trabalhar com muitos dados categóricos (mais de 1000 diferentes)
#plot(imoveis_analise$area, imoveis_analise$preco, )

summary(imoveis_analise$bairro)


#########################################################

#Cosas
#Business Understanding: De acordo com algumas características dads como número de dormitorios, banheiros, area, bairro e suites descobrir o preco do imovel

#Data Understanding

load("data/20140917_imoveis_completo.rda")
summary(imoveis)
imoveis$descricao <- NULL

#Data preparation
sum(is.na(imoveis))

#Valores NA são na verdade os zeros do dataset, portanto, transformando os NAs em zeros:
sum(imoveis$suites == "NA")
imoveis[is.na(imoveis)] <- 0

#removendo os valores em que preço está zerado
imoveis <- imoveis[imoveis$preco != 0, ]


#remover linha da suite com 822
imoveis <- imoveis[imoveis$suite != 822, ]
imoveis[which(imoveis$suite == 822), ]

#Preco maximo
nrow(imoveis[which(imoveis$preco > 14560000), ])
imoveis <- imoveis[imoveis$preco < 14560000, ]


#remover linha do banheiro 99
imoveis[which(imoveis$banheiro == 0), ]
imoveis <- imoveis[imoveis$banheiro != 99, ]
summary(imoveis)

#linha com 34 vagas
imoveis[which(imoveis$vagas > 10), ]
imoveis <- imoveis[imoveis$vagas != 34, ]


#remover area pequena
imoveis[which(imoveis$area < 20), ]
imoveis <- imoveis[imoveis$area > 7, ]


#dividir o dataset
ind <- sample(2, nrow(imoveis), replace = TRUE, prob = c(0.8,0.2))
train <- imoveis[ind == 1,]
test <- imoveis[ind == 2,]


#scatterplot

#library(psych)

#pairs.panels(imoveis)

#regressao

library(randomForest)

modellm <- lm(preco ~.,data=train)


# (6) validando o modelo completo no conjunto de teste
testPred <- predict(modellm, newdata = test)
summary(testPred)