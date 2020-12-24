library(tidyverse)
library(devtools)
library(petgg)
library(scales)
library(extrafont)

extrafont:: font_import()
extrafont::loadfonts()
petgg::set_theme(base_family = "EB Garamond")

#Criando os vetores taxa de pobreza linha US$5,50 PPC por dia

ano <- c(2012,2013,2014,2015,2016,2017,2018,2019)
tx_pob_no <- c(0.4251,0.4136,0.3827,0.4051,0.4370,0.4243,0.4127,0.4160)
tx_pob_to <- c(0.3867,0.3651,0.3184,0.3370,0.3378,0.3218,0.3154,0.3269)
tx_pob_br <- c(0.2646,0.2489,0.2280,0.2370,0.2551,0.2600,0.2528,0.2471)

tx_pob <- data.frame(ano, tx_pob_no, tx_pob_to, tx_pob_br)

#Criando o gr?fico


graf_taxas <- ggplot(tx_pob, aes(x = ano))+
  geom_line(aes(y = tx_pob_no, colour = "Norte"))+
  geom_line(aes(y = tx_pob_to, colour = "Tocantins"))+
  geom_line(aes(y = tx_pob_br, colour = "Brasil"))+
  scale_colour_manual(
    values = petgg::colors[1:3])+
  scale_y_continuous(labels=percent_format(accuracy = 1))+
  scale_x_continuous(labels = ano, breaks = ano)
graf_taxas

#Criando os vetores taxa de extrema pobreza US$1,90 PPC por dia

tx_expob_br <- c(0.0576,0.0510,0.0449,0.0489,0.0584,0.0644,0.0652,0.0654)
tx_expob_no <- c(0.0969,0.0834,0.0755,0.0904,0.0975,0.101,0.110,0.115)
tx_expob_to <- c(0.0559,0.0624,0.0514,0.0552,0.0578,0.0529,0.0659,0.0798)

tx_expob <- data.frame(ano, tx_expob_br, tx_expob_no, tx_expob_to)

#Criando o gr?fico

graf_taxas2 <- ggplot(tx_expob, aes(x = ano))+
  geom_line(aes(y = tx_expob_br, colour = "Brasil"))+
  geom_line(aes(y = tx_expob_no, colour = "Norte"))+
  geom_line(aes(y = tx_expob_to, colour = "Tocantins"))+
  scale_colour_manual(
    values = petgg::colors[1:3])+
  scale_y_continuous(labels=percent_format(accuracy = 1))+
  scale_x_continuous(labels = ano, breaks = ano)
graf_taxas2
#?ndice de Gini

gini_br <- c(0.540,0.533,0.526,0.524,0.537,0.538,0.545,0.543)
gini_no <- c(0.544,0.541,0.518,0.525,0.523,0.530,0.551,0.537)
gini_to <- c(0.509,0.514,0.495,0.504,0.489,0.497,0.528,0.530)

gini <- data.frame(ano, gini_br, gini_no, gini_to)

#Criando o gr?fico

graf_gini <- ggplot(gini, aes(x = ano))+
  geom_line(aes(y = gini_br, colour = "Brasil"))+
  geom_line(aes(y = gini_no, colour = "Norte"))+
  geom_line(aes(y = gini_to, colour = "Tocantins"))+
  scale_colour_manual(
    values = petgg::colors[1:3])+
  scale_x_continuous(labels = ano, breaks = ano)
graf_gini


#N?mero de pobres (linha US$5,50 PPC por dia)

pobs_br <- c(52262,49585,45816,48031,52153,53588,52522,51742)
pobs_no <- c(6986043,6894044,6471273,6949167,7597200,7474832,7369607,7533580)
pobs_to <- c(555166,530646,468778,502177,509155,490438,486184,509574)
pobs_pmw <- c(41321,61611,59853,42745,51228,57760,70886,56646)

pobs <- data.frame(ano, pobs_br, pobs_no, pobs_to, pobs_pmw)

#Criando o gr?fico

#Brasil
graf_pobs_br <- ggplot(pobs, aes(x = ano))+
  geom_line(aes(y = pobs_br), size = 1.5)+
  labs(x = "Ano", y = "N?mero de Pobres",
       title = "N?mero de Pobres no Brasil (1000 pessoas)",
       subtitle = "Linha de US$5,50 PPC por dia",
       caption = "Fonte: IBGE(2019)")
graf_pobs_br

#Norte
graf_pobs_no <- ggplot(pobs, aes(x = ano))+
  geom_line(aes(y = pobs_no), size = 1.5)+
  labs(x = "Ano", y = "N?mero de Pobres",
       title = "N?mero de Pobres no Norte",
       subtitle = "Linha de US$5,50 PPC por dia",
       caption = "Fonte: IBGE(2019)")
graf_pobs_no

#Tocantins
graf_pobs_to <- ggplot(pobs, aes(x = ano))+
  geom_line(aes(y = pobs_to), size = 1.5)+
  labs(x = "Ano", y = "N?mero de Pobres",
       title = "N?mero de Pobres no Tocantins",
       subtitle = "Linha de US$5,50 PPC por dia",
       caption = "Fonte: IBGE(2019)")
graf_pobs_to

#Palmas
graf_pobs_pmw <- ggplot(pobs, aes(x = ano))+
  geom_line(aes(y = pobs_pmw), size = 1.5)+
  labs(x = "Ano", y = "N?mero de Pobres",
       title = "N?mero de Pobres em Palmas",
       subtitle = "Linha de US$5,50 PPC por dia",
       caption = "Fonte: IBGE(2019)")
graf_pobs_pmw

#N?mero de extremamente pobres

expob_br <- c(11379,10153,9033,9917,11929,13268,13537,13689)
expob_no <- c(1592794,1389536,1276162,1551179,1694412,1780327,1969653,2073241)
expob_to <- c(80222,90667,75726,82297,87158,80683,101505,124457)
expob_pmw <- c(1860,7117,5401,9375,10024,5346,9691,12884)

expobs <- data.frame(ano, expob_br, expob_no, expob_to, expob_pmw)

#Criando os gr?ficos

#Brasil
graf_expobs_br <- ggplot(expobs, aes(x = ano))+
  geom_line(aes(y = expob_br), size = 1.5)+
  labs(x = "Ano", y = "N?mero de Extremamente Pobres",
       title = "N?mero de Extremamente Pobres no Brasil (1000 pessoas)",
       subtitle = "Linha de US$1,90 PPC por dia",
       caption = "Fonte: IBGE(2019)")
graf_expobs_br

#Norte
graf_expobs_no <- ggplot(expobs, aes(x = ano))+
  geom_line(aes(y = expob_no), size = 1.5)+
  labs(x = "Ano", y = "N?mero de Extremamente Pobres",
       title = "N?mero de Extremamente Pobres no Norte",
       subtitle = "Linha de US$1,90 PPC por dia",
       caption = "Fonte: IBGE(2019)")
graf_expobs_no

#Tocantins
graf_expobs_to <- ggplot(expobs, aes(x = ano))+
  geom_line(aes(y = expob_to), size = 1.5)+
  labs(x = "Ano", y = "N?mero de Extremamente Pobres",
       title = "N?mero de Extremamente Pobres no Tocantins",
       subtitle = "Linha de US$1,90 PPC por dia",
       caption = "Fonte: IBGE(2019)")
graf_expobs_to

#Palmas
graf_expobs_pmw <- ggplot(expobs, aes(x = ano))+
  geom_line(aes(y = expob_pmw), size = 1.5)+
  labs(x = "Ano", y = "N?mero de Extremamente Pobres",
       title = "N?mero de Extremamente Pobres em Palmas",
       subtitle = "Linha de US$1,90 PPC por dia",
       caption = "Fonte: IBGE(2019)")
graf_expobs_pmw

#Salvando as imagens

#ggsave(filename = "C:/Users/lucas/OneDrive/?rea de Trabalho/Boletim/pobreza.jpg", 
#      plot = graf_taxas)
#ggsave(filename = "C:/Users/lucas/OneDrive/?rea de Trabalho/Boletim/extrema_pobreza.jpg", 
#      plot = graf_taxas2)
#ggsave(filename = "C:/Users/lucas/OneDrive/?rea de Trabalho/Boletim/gini.jpg", 
#       plot = graf_gini)
#ggsave(filename = "C:/Users/lucas/OneDrive/?rea de Trabalho/Boletim/ren.jpg", 
#       plot = graf_ren_cor)
#ggsave(filename = "C:/Users/lucas/OneDrive/?rea de Trabalho/Boletim/pobres_br.jpg", 
#       plot = graf_pobs_br)
#ggsave(filename = "C:/Users/lucas/OneDrive/?rea de Trabalho/Boletim/pobres_no.jpg", 
#       plot = graf_pobs_no)
#ggsave(filename = "C:/Users/lucas/OneDrive/?rea de Trabalho/Boletim/pobres_to.jpg", 
#       plot = graf_pobs_to)
#ggsave(filename = "C:/Users/lucas/OneDrive/?rea de Trabalho/Boletim/pobres_pmw.jpg", 
#      plot = graf_pobs_pmw)
#ggsave(filename = "C:/Users/lucas/OneDrive/?rea de Trabalho/Boletim/expobres_br.jpg", 
#       plot = graf_expobs_br)
#ggsave(filename = "C:/Users/lucas/OneDrive/?rea de Trabalho/Boletim/expobres_no.jpg", 
#       plot = graf_expobs_no)
#ggsave(filename = "C:/Users/lucas/OneDrive/?rea de Trabalho/Boletim/expobres_to.jpg", 
#       plot = graf_expobs_to)
#ggsave(filename = "C:/Users/lucas/OneDrive/?rea de Trabalho/Boletim/expobres_pmw.jpg", 
#       plot = graf_expobs_pmw)