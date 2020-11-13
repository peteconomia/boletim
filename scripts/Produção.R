library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(odbc)
library(sidrar)
library(viridis)
library(hrbrthemes)
library(survival)
library(survminer)


#Produ??o - Janeiro
Dados <- get_sidra(x = 6588,
                   variable = 35,
                   period = "202001",
                   geo = "State",
                   header = TRUE,
                   format = 1)

Dados_TO <- Dados %>% 
  filter(`Unidade da Federa??o (C?digo)` == 17) %>% 
  filter(`Produto das lavouras (C?digo)` %in% c(39428, 39432, 39441, 39442, 39443, 39456, 39467)) 


#Produ??o - Fevereiro

Dados2 <- get_sidra(x = 6588,
                    variable = 35,
                    period = "202002",
                    geo = "State",
                    header = TRUE,
                    format = 1)

Dados_TO2 <- Dados2 %>% 
  filter(`Unidade da Federa??o (C?digo)` == 17) %>% 
  filter(`Produto das lavouras (C?digo)` %in% c(39428, 39432, 39441, 39442, 39443, 39456, 39467)) 

#Produ??o - Mar?o

Dados3 <- get_sidra(x = 6588,
                    variable = 35,
                    period = "202003",
                    geo = "State",
                    header = TRUE,
                    format = 1)

Dados_TO3 <- Dados3 %>% 
  filter(`Unidade da Federa??o (C?digo)` == 17) %>% 
  filter(`Produto das lavouras (C?digo)` %in% c(39428, 39432, 39441, 39442, 39443, 39456, 39467)) 

#Produ??o - Abril

Dados4 <- get_sidra(x = 6588,
                    variable = 35,
                    period = "202004",
                    geo = "State",
                    header = TRUE,
                    format = 1)

Dados_TO4 <- Dados4 %>% 
  filter(`Unidade da Federa??o (C?digo)` == 17) %>% 
  filter(`Produto das lavouras (C?digo)` %in% c(39428, 39432, 39441, 39442, 39443, 39456, 39467)) 


#Produ??o - Maio

Dados5 <- get_sidra(x = 6588,
                    variable = 35,
                    period = "202005",
                    geo = "State",
                    header = TRUE,
                    format = 1)

Dados_TO5 <- Dados5 %>% 
  filter(`Unidade da Federa??o (C?digo)` == 17) %>% 
  filter(`Produto das lavouras (C?digo)` %in% c(39428, 39432, 39441, 39442, 39443, 39456, 39467)) 

#Produ??o - Junho

Dados6 <- get_sidra(x = 6588,
                    variable = 35,
                    period = "202006",
                    geo = "State",
                    header = TRUE,
                    format = 1)

Dados_TO6 <- Dados6 %>% 
  filter(`Unidade da Federa??o (C?digo)` == 17) %>% 
  filter(`Produto das lavouras (C?digo)` %in% c(39428, 39432, 39441, 39442, 39443, 39456, 39467)) 


#Produ??o - Julho

Dados7 <- get_sidra(x = 6588,
                    variable = 35,
                    period = "202007",
                    geo = "State",
                    header = TRUE,
                    format = 1)

Dados_TO7 <- Dados7 %>% 
  filter(`Unidade da Federação (Código)` == 17) %>% 
  filter(`Produto das lavouras (Código)` %in% c(39428, 39432, 39441, 39442, 39443, 39456, 39467)) 


#Produção - Agosto

Dados8 <- get_sidra(x = 6588,
                    variable = 35,
                    period = "202008",
                    geo = "State",
                    header = TRUE,
                    format = 1)

Dados_TO8 <- Dados8 %>% 
  filter(`Unidade da Federação (Código)` == 17) %>% 
  filter(`Produto das lavouras (Código)` %in% c(39428, 39432, 39441, 39442, 39443, 39456, 39467)) 


#Produção - Setembro


Dados9 <- get_sidra(x = 6588,
                    variable = 35,
                    period = "202009",
                    geo = "State",
                    header = TRUE,
                    format = 1)


Dados_TO9 <- Dados9 %>% 
  filter(`Unidade da Federação (Código)` == 17) %>% 
  filter(`Produto das lavouras (Código)` %in% c(39428, 39432, 39441, 39442, 39443, 39456, 39467)) 





Total <-  data.frame("Meses" = 1:1, "Producao" = c(656.6,1205.5,3050.5,3045.8,239.5), "Produtos" = c("Arroz","Milho", "Soja", "Cana-de-açúcar", "Mandioca"))

Total %>%
  mutate(Produtos = factor(Produtos, levels=c("Arroz","Milho", "Soja", "Cana-de-açúcar", "Mandioca"))) %>%
  ggplot(aes(x=Produtos, y = Producao)) +
  geom_bar(stat='identity', width=0.5, fill = "#4682B4") + 
  theme_classic() + 
  geom_text(aes(label=Producao), position=position_dodge(width=0.9), vjust=-0.25) +
  scale_y_continuous(limits = c(0, 4000)) + 
  labs( y= "Produção em mil (toneladas)", x= " ") + 
  theme(plot.title = element_text(hjust = 0.5, size = 13))



plot = ggplot(Total, aes(x=Produtos, y=Producao, fill=Produtos))

plot + geom_area(stat = "identity", position = "stack")



