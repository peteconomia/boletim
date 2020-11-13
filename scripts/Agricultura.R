library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(odbc)
library(sidrar)

 
#Area Plantada - Janeiro
Dados <- get_sidra(x = 6588,
          variable = 109,
          period = "202001",
          geo = "State",
          header = TRUE,
          format = 1)

Dados_TO <- Dados %>% 
  filter(`Unidade da Federação (Código)` == 17) %>% 
  filter(`Produto das lavouras (Código)` %in% c(39428, 39432, 39441, 39442, 39443, 39456, 39467)) 

#Area Plantada - Fevereiro


Dados2 <- get_sidra(x = 6588,
                   variable = 109,
                   period = "202002",
                   geo = "State",
                   header = TRUE,
                   format = 1)

Dados_TO2 <- Dados2 %>% 
  filter(`Unidade da Federação (Código)` == 17) %>% 
  filter(`Produto das lavouras (Código)` %in% c(39428, 39432, 39441, 39442, 39443, 39456, 39467)) 


#Area Plantada - Março

Dados3 <- get_sidra(x = 6588,
                    variable = 109,
                    period = "202003",
                    geo = "State",
                    header = TRUE,
                    format = 1)


Dados_TO3 <- Dados3 %>% 
  filter(`Unidade da Federação (Código)` == 17) %>% 
  filter(`Produto das lavouras (Código)` %in% c(39428, 39432, 39441, 39442, 39443, 39456, 39467)) 

#Area Plantada - Abril

Dados4 <- get_sidra(x = 6588,
                    variable = 109,
                    period = "202004",
                    geo = "State",
                    header = TRUE,
                    format = 1)


Dados_TO4 <- Dados4 %>% 
  filter(`Unidade da Federação (Código)` == 17) %>% 
  filter(`Produto das lavouras (Código)` %in% c(39428, 39432, 39441, 39442, 39443, 39456, 39467))


#Area Plantada - Maio

Dados5 <- get_sidra(x = 6588,
                    variable = 109,
                    period = "202005",
                    geo = "State",
                    header = TRUE,
                    format = 1)


Dados_TO5 <- Dados5 %>% 
  filter(`Unidade da Federação (Código)` == 17) %>% 
  filter(`Produto das lavouras (Código)` %in% c(39428, 39432, 39441, 39442, 39443, 39456, 39467))


#Area Plantada - Junho
Dados61 <- get_sidra(x = 6588,
                    variable = 109,
                    period = "202006",
                    geo = "State",
                    header = TRUE,
                    format = 1)

Dados_TO62 <- Dados61 %>% 
  filter(`Unidade da Federação (Código)` == 17) %>% 
  filter(`Produto das lavouras (Código)` %in% c(39428, 39432, 39441, 39442, 39443, 39456, 39467))

#Area Plantada - Julho

Dados7 <- get_sidra(x = 6588,
                    variable = 109,
                    period = "202007",
                    geo = "State",
                    header = TRUE,
                    format = 1)

Dados_TO7 <- Dados7 %>% 
  filter(`Unidade da Federação (Código)` == 17) %>% 
  filter(`Produto das lavouras (Código)` %in% c(39428, 39432, 39441, 39442, 39443, 39456, 39467))



#Gráficos - Área Plantada por hectares

Total <-  data.frame("Meses" = 1:5, "Área Plantada" = c(124.8,266.5,975.5,38.4,14.9), "Produtos" = c("Arroz","Milho", "Soja", "Cana-de-açucar", "Mandioca"))


Total %>%
  mutate(Produtos = factor(Produtos, levels=c("Arroz","Milho", "Soja", "Cana-de-açucar", "Mandioca"))) %>%
  ggplot(aes(x=Produtos, y = Área.Plantada)) +geom_bar(stat='identity', width=0.5, fill = "#4682B4") + 
  theme_classic() + geom_text(aes(label=Área.Plantada), position=position_dodge(width=0.9), vjust=-0.25) + scale_y_continuous(limits = c(0, 1000)) + labs(title="Área Plantada até Junho", y= "Área Plantada por mil Hectare", x= "Produtos", caption = "Fonte: IBGE \n Elaborado por: PET Economia") + theme(plot.title = element_text(hjust = 0.5, size = 13))


