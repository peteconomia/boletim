library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(odbc)
library(sidrar)



#Area Colhida - Janeiro
Dados <- get_sidra(x = 6588,
                   variable = 216,
                   period = "202001",
                   geo = "State",
                   header = TRUE,
                   format = 1)

Dados_TO <- Dados %>% 
  filter(`Unidade da Federa??o (C?digo)` == 17) %>% 
  filter(`Produto das lavouras (C?digo)` %in% c(39428, 39432, 39441, 39442, 39443, 39456, 39467)) 


#Area Colhida - Fevereiro

Dados2 <- get_sidra(x = 6588,
                   variable = 216,
                   period = "202002",
                   geo = "State",
                   header = TRUE,
                   format = 1)

Dados_TO2 <- Dados2 %>% 
  filter(`Unidade da Federa??o (C?digo)` == 17) %>% 
  filter(`Produto das lavouras (C?digo)` %in% c(39428, 39432, 39441, 39442, 39443, 39456, 39467)) 

#Area Colhida - Mar?o

Dados3 <- get_sidra(x = 6588,
                    variable = 216,
                    period = "202003",
                    geo = "State",
                    header = TRUE,
                    format = 1)

Dados_TO3 <- Dados3 %>% 
  filter(`Unidade da Federa??o (C?digo)` == 17) %>% 
  filter(`Produto das lavouras (C?digo)` %in% c(39428, 39432, 39441, 39442, 39443, 39456, 39467)) 

#Area Colhida - Abril

Dados4 <- get_sidra(x = 6588,
                    variable = 216,
                    period = "202004",
                    geo = "State",
                    header = TRUE,
                    format = 1)

Dados_TO4 <- Dados4 %>% 
  filter(`Unidade da Federa??o (C?digo)` == 17) %>% 
  filter(`Produto das lavouras (C?digo)` %in% c(39428, 39432, 39441, 39442, 39443, 39456, 39467)) 


#Area Colhida - Maio

Dados5 <- get_sidra(x = 6588,
                    variable = 216,
                    period = "202005",
                    geo = "State",
                    header = TRUE,
                    format = 1)

Dados_TO5 <- Dados5 %>% 
  filter(`Unidade da Federa??o (C?digo)` == 17) %>% 
  filter(`Produto das lavouras (C?digo)` %in% c(39428, 39432, 39441, 39442, 39443, 39456, 39467)) 

#Area Colhida - Junho

Dados6 <- get_sidra(x = 6588,
                    variable = 216,
                    period = "202006",
                    geo = "State",
                    header = TRUE,
                    format = 1)

Dados_TO6 <- Dados6 %>% 
  filter(`Unidade da Federa??o (C?digo)` == 17) %>% 
  filter(`Produto das lavouras (C?digo)` %in% c(39428, 39432, 39441, 39442, 39443, 39456, 39467)) 


#Area Colhida - Julho

Dados7 <- get_sidra(x = 6588,
                    variable = 216,
                    period = "202007",
                    geo = "State",
                    header = TRUE,
                    format = 1)

Dados_TO7 <- Dados7 %>% 
  filter(`Unidade da Federação (Código)` == 17) %>% 
  filter(`Produto das lavouras (Código)` %in% c(39428, 39432, 39441, 39442, 39443, 39456, 39467)) 

# Área colhida - Agosto

Dados8 <- get_sidra(x = 6588,
                    variable = 216,
                    period = "202008",
                    geo = "State",
                    header = TRUE,
                    format = 1)

Dados_TO8 <- Dados7 %>% 
  filter(`Unidade da Federação (Código)` == 17) %>% 
  filter(`Produto das lavouras (Código)` %in% c(39428, 39432, 39441, 39442, 39443, 39456, 39467)) 


# Área colhida - Setembro


Dados9 <- get_sidra(x = 6588,
                    variable = 216,
                    period = "202009",
                    geo = "State",
                    header = TRUE,
                    format = 1)

Dados_TO9 <- Dados9 %>% 
  filter(`Unidade da Federação (Código)` == 17) %>% 
  filter(`Produto das lavouras (Código)` %in% c(39428, 39432, 39441, 39442, 39443, 39456, 39467)) 



#Gr?ficos - ?rea Colhida por hectares

Total <-  data.frame("Meses" = 1:5, "Area colhida" = c(126.6,276.7,990.8,38.4,15.0), "Produtos" = c("Arroz","Milho", "Soja", "Cana-de-açúcar", "Mandioca"))


Total %>%
  mutate(Produtos = factor(Produtos, levels=c("Arroz","Milho", "Soja", "Cana-de-açúcar", "Mandioca"))) %>%
  ggplot(aes(x=Produtos, y = Area.colhida)) +
  geom_text(aes(label=Area.colhida), position=position_dodge(width=0.9), vjust=-0.25) +geom_bar(stat='identity', width=0.5, fill = "#4682B4") + 
  theme_classic()  + 
  labs(title = ) + 
  scale_y_continuous(limits = c(0, 1000)) + 
  labs(y= "Área Plantada por mil Hectare", x= " " ) + theme(plot.title = element_text(hjust = 0.5, size = 13))





