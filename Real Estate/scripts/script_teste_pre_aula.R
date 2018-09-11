load(file = '../data/20140917_imoveis_completo.rda')

imoveis$descricao <- NULL

imoveis <- imoveis[imoveis$preco < 5000000, ]

imoveis <- imoveis[imoveis$area > 10, ]
