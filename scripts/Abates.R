library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(sidrar)

#Diret?rio 
setwd("C:/Users/ferre/OneDrive/?rea de Trabalho/UFT/Boletim - PET/Dados Agricultura")

#Dados

Dados <- get_sidra(x = 1092,
                   variable = 284,
                   period = "202001",
                   geo = "State",
                   header = TRUE,
                   format = 1)

Dados_Tocantins <- Dados %>% 
  filter(`Unidade da Federa??o (C?digo)` == 17) %>% 
  filter(`Tipo de inspe??o (C?digo)` == 118225) %>% 
  filter(`Refer?ncia temporal (C?digo)` == 115236)



#Gr?ficos sobre Abate - Total.

Total <-  data.frame("Per?odo" = 1:5, "Abates" = c(286.292,265.205,268.440,212.620,230.200), "Meses" = c("1T - 2019","2T - 2019", "3T - 2019", "4T - 2019", "1T - 2020"))

Total %>%
  mutate(Meses = factor(Meses, levels=c("1T - 2019","2T - 2019", "3T - 2019", "4T - 2019", "1T - 2020"))) %>%
  ggplot(aes(x=Meses, y = Abates, group = 1)) + geom_bar(stat = "identity", width = 0.6, fill = "#4682B4")+
  theme_classic()   + 
  scale_y_continuous(limits = c(0, 500), breaks = seq(from = 0, to = 500, by = 75)) + 
  labs(y= "Abate total por mil", x=" "  ) + theme(plot.title = element_text(hjust = 0.5, size = 13)) +
  geom_text(aes(label=Abates), position=position_dodge(width=0.9), vjust=-0.25)

#Gr?ficos sobre Bois - Total.

Bois <-  data.frame("Per?odo" = 1:5, "Bois" = c(145.567,148.066,186.745,155.345,152.997), "Meses" = c("1T - 2019","2T - 2019", "3T - 2019", "4T - 2019", "1T - 2020"))

Bois %>%
  mutate(Meses = factor(Meses, levels=c("1T - 2019","2T - 2019", "3T - 2019", "4T - 2019", "1T - 2020"))) %>%
  ggplot(aes(x=Meses, y = Bois, group = 1)) + geom_bar(stat = "identity", width = 0.5, fill = "#4682B4")+
  theme_classic()   + 
  scale_y_continuous(limits = c(0, 300), breaks = seq(from = 0, to = 300, by = 50)) + 
  labs(y= "Abate total por mil", x=" " ) + theme(plot.title = element_text(hjust = 0.5, size = 13))+
  geom_text(aes(label=Bois), position=position_dodge(width=0.9), vjust=-0.25)


#Gr?ficos sobre Vacas - Total.

Vacas <-  data.frame("Per?odo" = 1:5, "Vacas" = c(131.941,109.496,74.619,46.559,66.428), "Meses" = c("1T - 2019","2T - 2019", "3T - 2019", "4T - 2019", "1T - 2020"))

Vacas %>% 
  mutate(Meses = factor(Meses, levels=c("1T - 2019","2T - 2019", "3T - 2019", "4T - 2019", "1T - 2020"))) %>%
  ggplot(aes(x=Meses, y = Vacas, group = 1)) + geom_bar(stat = "identity", width = 0.5, fill = "#4682B4")+
  theme_classic()   + 
  scale_y_continuous(limits = c(0, 300) , breaks = seq(from = 0, to = 300, by = 50)) + 
  labs(x=" ", y= "Abate total por mil") + theme(plot.title = element_text(hjust = 0.5, size = 13))+
  geom_text(aes(label=Vacas), position=position_dodge(width=0.9), vjust=-0.25)

#Gr?fico sobre Novilhas 

Novilhas <-  data.frame("Per?odo" = 1:5, "Novilhas" = c(8.784,7.643,7.076,10.716,10.775), "Meses" = c("1T - 2019","2T - 2019", "3T - 2019", "4T - 2019", "1T - 2020"))

Novilhas %>% 
  mutate(Meses = factor(Meses, levels=c("1T - 2019","2T - 2019", "3T - 2019", "4T - 2019", "1T - 2020"))) %>%
  ggplot(aes(x=Meses, y = Novilhas, group = 1)) + geom_bar(stat = "identity", width = 0.5, fill = "#4682B4")+
  theme_classic()   + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(from = 0, to = 100, by = 25)) + 
  labs(x = " ",y= "Abate total por mil") + theme(plot.title = element_text(hjust = 0.5, size = 13)) +
  geom_text(aes(label=Novilhas), position=position_dodge(width=0.9), vjust=-0.25)


#Gr?fico sobre Frangos.

Frangos <-  data.frame("Per?odo" = 1:5, "Frangos" = c(0,3949.592,4313.581,3767.522,4574.260), "Meses" = c("1T - 2019","2T - 2019", "3T - 2019", "4T - 2019", "1T - 2020"))

Frangos %>% 
  mutate(Meses = factor(Meses, levels=c("1T - 2019","2T - 2019", "3T - 2019", "4T - 2019", "1T - 2020"))) %>%
  ggplot(aes(x=Meses, y = Frangos)) + geom_bar(stat = "identity", width = 0.5, fill = "#4682B4")+
  theme_classic()   + 
  scale_y_continuous(limits = c(0, 5000), breaks = seq(from = 0, to = 5000, by = 500)) + 
  labs(x=" ", y= "Abate total por mil") + theme(plot.title = element_text(hjust = 0.5, size = 13)) + 
  geom_text(aes(label=Frangos), position=position_dodge(width=0.9), vjust=-0.25)


#Gr?fico sobre o Primeiro Trimestre


Dados_Totais <-  data.frame("Per?odo" = 1:4, "Produtos" = c(152.997,66.428,10.775,4574.260), "Abates" = c("Bois","Vacas", "Novilhas", "Frangos"))

Dados_Totais %>% 
  mutate(Abates = factor(Abates, levels=c("Bois","Vacas", "Novilhas", "Frangos"))) %>%
  ggplot(aes(x=Abates, y = Produtos)) + geom_bar(stat = "identity", width = 0.5, fill = "#4682B4")+
  theme_classic()   + 
  scale_y_continuous(limits = c(0, 5000), breaks = seq(from = 0, to = 5000, by = 500)) + 
  labs(x=" ", y= "Abate total por mil") + theme(plot.title = element_text(hjust = 0.5, size = 13)) +
  geom_text(aes(label=Produtos), position=position_dodge(width=0.9), vjust=-0.25)

  