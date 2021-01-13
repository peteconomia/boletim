# Pacotes 

library(RCurl)
library(tidyverse)
library(survey)
library(convey)
library(PNADcIBGE)
library(ecoseries)
library(ipeadatar)
library(devtools)
library(RCurl)
library(readr)
library(timeDate)
library(safejoin)
library(petgg)
library(extrafont)


# Saldo do Tocantins

#Dados

Tocantins<- data.frame("Empregos" = 1:10, "Saldo" = c(199,1482,-86,-2850,-1251,1240,1810,2096,1790,1504), "Meses" = c("Jan","Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out"))


#Gráfico

Tocantins %>%
  mutate(Meses = factor(Meses, levels=c("Jan","Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out"))) %>%
  ggplot(aes(x=Meses, y = Saldo)) + 
  geom_bar(stat='identity', position = "dodge" ) +
  scale_y_continuous(limits = c(-4500, 4500), breaks = seq(from = -4000, to = 4000, by = 1000)) + 
  theme(plot.title = element_text(hjust = 0.5, size = 13)) +
  labs(x= " ", y = " ")

# Saldo Região NORTE


#Dados

Norte <- data.frame("Empregos" = 1:10, "Saldo" = c(2764,10419,-6370,-30747,-11146,6093,16015,22483,20640, 20658), "Meses" = c("Jan","Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out"))

#Gráfico

Norte %>%
  mutate(Meses = factor(Meses, levels=c("Jan","Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out"))) %>%
  ggplot(aes(x=Meses, y = Saldo)) + 
  geom_bar(stat='identity') + 
  scale_y_continuous(limits = c(-35000, 25000), breaks = seq(from = -35000, to = 25000, by = 10000))  


# Série de Empregos - Tocantins - Trimestral

# Dados

Saldo_Total_TO <-  data.frame("Empregos" = 1:6, "Saldo" = c(324,141,1226,-1336,1595,-2861), "Trimestres" = c("1T/2019","2T/2019", "3T/2019", "4T/2019", "1T/2020","2T/2020"))

#  Gráfico


Saldo_Total_TO %>%
  mutate(Trimestres = factor(Trimestres, levels=c("1T/2019","2T/2019", "3T/2019", "4T/2019", "1T/2020","2T/2020"))) %>%
  ggplot(aes(x=Trimestres, y = Saldo)) + 
  geom_bar(stat='identity') + 
  scale_y_continuous(limits = c(-3500, 3500), breaks = seq(from = -3500, to = 3500, by = 1000))  


 # Série de Empregos - Região Norte - Trimestral

  # Dados

Saldo_TotalN <-  data.frame("Empregos" = 1:6, "Saldo" = c(-8175,11204,27053,-5456,6813,-35800), "Trimestres" = c("1T/2019","2T/2019", "3T/2019", "4T/2019", "1T/2020","2T/2020"))

    # Gráficos

Saldo_TotalN %>%
  mutate(Trimestres = factor(Trimestres, levels=c("1T/2019","2T/2019", "3T/2019", "4T/2019", "1T/2020","2T/2020"))) %>%
  ggplot(aes(x=Trimestres, y = Saldo)) + 
  geom_bar(stat='identity') + 
  scale_y_continuous(limits = c(-45000, 45000), breaks = seq(from = -45000, to = 45000, by = 10000))  


    # Saldo por Setores


  # Dados


    Setores <- data.frame("Empregos" = 1:5, "Saldo" = c(277,109,961,-1950,-660), "Setores" = c("Agricultura","Indústrias", "Construção", "Comércio", "Serviços"))


    
    # Gráficos
    
    Setores %>%
      mutate(Setores = factor(Setores, levels=c("Agricultura","Indústrias", "Construção", "Comércio", "Serviços"))) %>%
      ggplot(aes(x=Setores, y = Saldo)) + 
      geom_bar(stat='identity') +
      scale_y_continuous(limits = c(-2500, 2500),breaks = seq(from = -2000, to = 2500, by = 500)) + 
      labs(x=" ") 
    
    
    
    # Saldo por Idade
    
    
    # Dados
    
    Idade <- data.frame("Empregos" = 1:3, "Saldo" = c(-254, -1810, -63), "Idade" = c("14-34", "35-64", "65+"))
    

    # Gráficos
    
    Idade %>%
      mutate(Setores = factor(Idade, levels=c("14-34", "35-64", "65+"))) %>%
      ggplot(aes(x=Idade, y = Saldo)) + 
      geom_bar(stat='identity') +
      scale_y_continuous(limits = c(-2000, 2500),breaks = seq(from = -2000, to = 2500, by = 500)) + 
      theme(plot.title = element_text(hjust = 0.5, size = 2)) + 
      labs(x=" ")
    
    
    
    # Saldo por Genêro
    
    
    # Dados
    
    Sexo <- data.frame("Genero" = 1:2, "Saldo" = c(182,-1448), "Sexo" = c("Homem", "Mulher"))
    
    
    
    # Gráficos
    
    
    Sexo %>%
      mutate(Sexo = factor(Sexo, levels=c("Homem", "Mulher"))) %>%
      ggplot(aes(x=Sexo, y = Saldo)) + 
      geom_bar(stat='identity') +
      scale_y_continuous(limits = c(-2000, 1000),breaks = seq(from = -2000, to = 2500, by = 500)) + 
      theme(plot.title = element_text(hjust = 0.5, size = 13)) +  
      labs(x = " ") 
    
    
    # Saldo por etnia
    
    #Dados
    
    Etnia <- data.frame("Raça" = 1:6, "Saldo" = c(-443, -75, -777,-72,630, -527), "Etnia" = c("Branca", "Preta", "Parda", "Amarela", "N/Informado", "N/indentificado"))
    
    
    # Gráficos 
    
    m <- 1000
    
    
    Etnia %>%
      mutate(Etnia = factor(Etnia, levels=c("Branca", "Preta", "Parda", "Amarela", "N/Informado", "N/indentificado"))) %>%
      ggplot(aes(x=Etnia, y = Saldo)) + 
      geom_bar(stat='identity', position = "dodge") +
      scale_y_continuous(labels = function(n) n/m) + 
      labs(x= " ")
    
    
    # Taxa de Desemprego Tocantins
    
    # Dados
    
    # Usar o pacote do IPEA
    
    data<-ipeadata(c("PNADC_TXDES_UF"))
    
    dados_to  <- data %>% 
      filter(tcode == 17)
    
    
    # Gráficos
    
    dados_to %>% 
      ggplot(aes(x = date, y = value, group = 1))  +
      geom_line(size = 0.9)  +  
      scale_y_continuous(limits = c(0, 15), breaks = seq(from = -0, to = 15, by = 3)) + 
      labs( x= "Período" , y= "Taxa") + 
      theme(plot.title = element_text(hjust = 0.5, size = 13)) + 
      geom_rect(  xmin = as.Date("2019-01-01"),
                  xmax = as.Date("2019-04-01"),
                  ymin = -Inf,
                  ymax = Inf,
                  # deixar o retangulo mais transparente
                  alpha = 0.01) +
      geom_rect(xmin = as.Date("2019-10-01"),
                xmax = as.Date("2020-01-01"),
                ymin = -Inf,
                ymax = Inf,
                # deixar o retangulo mais transparente
                alpha = 0.01) + 
      geom_text(aes(label = value))
    
    
    # Pedidos Seguro desemprego
    
    # Dados 
    
    y <- c(9833, 9833, 8832, 8784, 9642, 9324, 9155, 8182, 8785, 10642)
    x <- c("1T/2018","2T/2018", "3T/2018", "4T/2018", "1T/2019","2T/2019", "3T/2019", "4T/2019","1T/2020","2T/2020")
    
    dados <- data.frame(x,y)
    
    
    # Gráficos
    
    dados %>% 
      mutate(x = factor(x, levels=c("1T/2018","2T/2018", "3T/2018", "4T/2018", "1T/2019","2T/2019", "3T/2019", "4T/2019","1T/2020","2T/2020"))) %>%
      ggplot(aes(x=x, y = y, group = 1)) +
      geom_bar(stat = "identity") +
      scale_y_continuous(limits = c(0, 15000), breaks = seq(from = 0, to = 15000, by = 2500)) + 
      labs( x= " ", y= "Seguro Desemprego") + 
      theme(plot.title = element_text(hjust = 0.5, size = 13)) +
      geom_text(aes(label = y, y = y + 3),  position = position_dodge(width =  0.9), vjust = -0.30)
    
    
    # Relação Pedidos Seguro Desemprego x Taxa de desemprego
    
    # Dados/Exemplo
    
    
    Dados <- data.frame(taxaDesemprego = c(11,11.3,9.8,10.4,12.3,11.4,10.5,9.1,11.2,12.5),
                        Pedidos_SeguroDesemprego = c(9833,9833,8832,8784,9642,9324,9155,8182,8785,10642))
    
    ggplot(Dados, aes(x = taxaDesemprego, y = Pedidos_SeguroDesemprego)) + geom_point()
    
    
    #Modelo/Gráfico
    
    modelo <- lm(data = Dados, formula = Pedidos_SeguroDesemprego~taxaDesemprego)
    modelo$coefficients
    summary(modelo)
    
    dados <- data.frame(x,y)
    
    ggplot(Dados, aes(x = taxaDesemprego, y = Pedidos_SeguroDesemprego)) + geom_point() + geom_smooth(method = lm, se = FALSE) +
      labs(x= "Taxa de desemprego", y= "Seguro Desemprego") + theme(plot.title = element_text(hjust = 0.5, size = 13)) + theme_petgg(axis_title = T)
    
    
    # Testes 
    
    shapiro.test(modelo$residuals)
    
    
    # População Ocupada - Tocantins
    
    
    # Dados - Usando o IPEA
    
    data_populaçãoOCUP <- ipeadata(c("PNADC_TXPARTFT_UF"))
    
    Dados_TOCANTINS <- data_populaçãoOCUP %>% 
      filter(tcode == 17)
    
    
    # Gráficos
    
    Dados_TOCANTINS %>% 
      ggplot(aes(x = date, y = value, group = 1))  +
      geom_line(size = 0.9)  +  
      scale_y_continuous(limits = c(0, 70), breaks = seq(from = -0, to = 70, by = 20)) + 
      labs(x= " " , y= "Taxa") + 
      theme(plot.title = element_text(hjust = 0.5, size = 6)) + 
      geom_rect(  xmin = as.Date("2019-01-01"),
                  xmax = as.Date("2019-04-01"),
                  ymin = -Inf,
                  ymax = Inf,
                  # deixar o retangulo mais transparente
                  alpha = 0.01) +
      geom_rect(xmin = as.Date("2019-10-01"),
                xmax = as.Date("2020-01-01"),
                ymin = -Inf,
                ymax = Inf,
                # deixar o retangulo mais transparente
                alpha = 0.01) 
    
    
    # Rendimento médio - Tocantins
    
    
    # Dados
    
    rendimento_TO <- ipeadata(c("PNADC_RENDREALPRINCHAB_UF"))
    
    rendimento_tocantins <- rendimento_TO %>% 
      filter(tcode == 17)
    
    # Gráficos
    
    rendimento_tocantins %>% 
      ggplot(aes(x = date, y = value, group = 1))  +
      geom_line(size = 0.9)  +  
      scale_y_continuous(limits = c(0, 3500), breaks = seq(from = -0, to = 3500, by = 500)) + 
      labs(x= " " , y= "Rendimento") + 
      theme(plot.title = element_text(hjust = 0.5, size = 13)) + 
      geom_rect(  xmin = as.Date("2019-01-01"),
                  xmax = as.Date("2019-04-01"),
                  ymin = -Inf,
                  ymax = Inf,
                  # deixar o retangulo mais transparente
                  alpha = 0.01) +
      geom_rect(xmin = as.Date("2019-10-01"),
                xmax = as.Date("2020-01-01"),
                ymin = -Inf,
                ymax = Inf,
                # deixar o retangulo mais transparente
                alpha = 0.01) 
    
    
    # Rendimento médio - Região Norte 
    
    # Dados
    
    rendimento_TO <- ipeadata(c("PNADC_RENDREALPRINCHAB_UF"))
    
    
    rendimento_norte <- rendimento_TO %>% 
      filter(tcode == 1)
    
    # Gráficos 
    
    rendimento_norte %>% 
      ggplot(aes(x = date, y = value, group = 1))  +
      geom_line(size = 0.9)  +  
      scale_y_continuous(limits = c(0, 3500), breaks = seq(from = -0, to = 3500, by = 500)) + 
      labs(x= " " , y= "Rendimento") + 
      theme(plot.title = element_text(hjust = 0.5, size = 13)) + 
      geom_rect(  xmin = as.Date("2019-01-01"),
                  xmax = as.Date("2019-04-01"),
                  ymin = -Inf,
                  ymax = Inf,
                  # deixar o retangulo mais transparente
                  alpha = 0.01) +
      geom_rect(xmin = as.Date("2019-10-01"),
                xmax = as.Date("2020-01-01"),
                ymin = -Inf,
                ymax = Inf,
                # deixar o retangulo mais transparente
                alpha = 0.01) 
    
    
    # Rendimento médio - Brasil
    
    # Dados
    
    rendimento_TO <- ipeadata(c("PNADC_RENDREALPRINCHAB_UF"))
    
    rendimento_br <- rendimento_TO %>% 
      filter(tcode == 0)
    
    
    # Gráficos
    
    rendimento_br %>% 
      ggplot(aes(x = date, y = value, group = 1))  +
      geom_line(size = 0.9) +
      scale_y_continuous(limits = c(0, 3500), breaks = seq(from = -0, to = 3500, by = 500)) + 
      labs(x= " " , y= "Rendimento") + 
      theme(plot.title = element_text(hjust = 0.5, size = 13)) + 
      geom_rect(  xmin = as.Date("2019-01-01"),
                  xmax = as.Date("2019-04-01"),
                  ymin = -Inf,
                  ymax = Inf,
                  # deixar o retangulo mais transparente
                  alpha = 0.01) +
      geom_rect(xmin = as.Date("2019-10-01"),
                xmax = as.Date("2020-01-01"),
                ymin = -Inf,
                ymax = Inf,
                # deixar o retangulo mais transparente
                alpha = 0.01) 
    