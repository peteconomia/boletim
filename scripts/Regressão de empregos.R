library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)

#Exemplo


Dados <- data.frame(taxaDesemprego = c(11,11.3,9.8,10.4,12.3,11.4,10.5,9.1,11.2,12.5),
                    Pedidos_SeguroDesemprego = c(9833,9833,8832,8784,9642,9324,9155,8182,8785,10642))

ggplot(Dados, aes(x = taxaDesemprego, y = Pedidos_SeguroDesemprego)) + geom_point()

#Modelo

modelo <- lm(data = Dados, formula = Pedidos_SeguroDesemprego~taxaDesemprego)
modelo$coefficients
summary(modelo)

dados <- data.frame(x,y)

ggplot(Dados, aes(x = taxaDesemprego, y = Pedidos_SeguroDesemprego)) + geom_point() + geom_smooth(method = lm, se = FALSE, fill = "#4682B4") + theme_classic() +
  labs(x= "Taxa de desemprego", y= "Seguro Desemprego") + theme(plot.title = element_text(hjust = 0.5, size = 13))

shapiro.test(modelo$residuals)

cor(Dados, x =taxaDesemprego, y = Pedidos_SeguroDesemprego)



#Modelo 10 - Regressão - Erro.

Dados <- data.frame(Saldo_Caged = c(-429, 1116, -363, 144, 44, -47, -16, 818, 424, 509, -115, -1730, 212, 1446, -60, -2061, -1206, 1202, 1409),
                    Taxa_Desemprego = c(9.0,10.7,11.2,10.8,13.1,12.6,11.7,11.8,10.5,11,11.3,9.8,10.4,12.3,11.4,10.5,9.1,11.2,12.5))

ggplot(Dados, aes(x = Taxa_Desemprego, y = Saldo_Caged)) + geom_point()

modelo <- lm(data = Dados, formula = Saldo_Caged~Taxa_Desemprego)

modelo$coefficients

summary(modelo)

ggplot(Dados, aes(x = Taxa_Desemprego, y = Saldo_Caged)) + geom_point() + geom_smooth(method = lm, se = FALSE, fill = "#4682B4") + theme_classic() +
  labs(title="Relação Taxa de Desemprego x Saldo do Caged - TO", x= "Taxa de desemprego", y= "Seguro Desemprego",  caption = "Fonte: Ministério do Trabalho \n Elaborado por: PET Economia") + theme(plot.title = element_text(hjust = 0.5, size = 13))



